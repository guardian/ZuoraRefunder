import okhttp3._
import com.github.nscala_time.time.Imports._
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Adjuster extends App with LazyLogging {

  val username = System.getenv("Z_USER")
  val password = System.getenv("Z_PASS")

  case class Sub(
                  subName: String,
                  miniPaymentAmount: BigDecimal,
                  oldAmount: BigDecimal,
                  newAmount: BigDecimal,
                  nextInvoiceDate: DateTime
                )

  implicit val updateSubscriptionReads: Reads[UpdateSubscriptionResponse] = (
    (JsPath \ "success").read[Boolean] and
    (JsPath \ "subscriptionId").read[String]
  )(UpdateSubscriptionResponse.apply _)

  case class UpdateSubscriptionResponse(
                          success: Boolean,
                          subscriptionId: String
                          )

  case class Adjustment(
                         productRatePlanId: String,
                         productRatePlanChargeId: String
                       )

  val UATADJUSTMENT = Adjustment(
    productRatePlanId = "2c92c0f85e0d9c02015e0e527a5e7120",
    productRatePlanChargeId = "2c92c0f95e0da917015e0e54be576690"
  )

  val client = new OkHttpClient()

  val mediaType = MediaType.parse("application/json")

  def dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

  def formatDate(d: DateTime): String = dateFormatter.print(d)

  def formatCurrency(m: BigDecimal): String = m.setScale(2, BigDecimal.RoundingMode.HALF_UP).toString

  def formatDiscount(m: BigDecimal): String = s"-${formatCurrency(m)}"

  def adjustSub(adjustment: Adjustment)(sub: Sub) = {
    logger.info(s"Attempting to adjust: ${sub.subName}")
    val ongoingAdjustmentStartDate = formatDate(sub.nextInvoiceDate)

    val miniPaymentAdjustmentStartDate = formatDate(sub.nextInvoiceDate.minusMonths(1))
    val miniPaymentAdjustmentEndDate = formatDate(sub.nextInvoiceDate) //Exclusive end date not inclusive

    val ongoingAdjustmentAmount = sub.newAmount - sub.oldAmount
    val formattedOngoingAdjustmentAmount = formatDiscount(ongoingAdjustmentAmount)

    val miniPaymentAmount = sub.miniPaymentAmount
    val formattedMiniPaymentAmount = formatDiscount(miniPaymentAmount)
    logger.info(
      s"Subscription: ${sub.subName} \tOngoing Discount Start Date: ${ongoingAdjustmentStartDate} \tOngoing Discount Amount: ${formattedOngoingAdjustmentAmount} \tMini Payment Amount: ${formattedMiniPaymentAmount} \tMini Payment From ${miniPaymentAdjustmentStartDate} until $miniPaymentAdjustmentEndDate")

    val json =
      s"""
         |{
         |	"add": [
         |		{
         |			"contractEffectiveDate": "$miniPaymentAdjustmentStartDate",
         |			"customerAcceptanceDate": "$miniPaymentAdjustmentStartDate",
         |			"serviceActivationDate": "$miniPaymentAdjustmentStartDate",
         |			"productRatePlanId": "${adjustment.productRatePlanId}",
         |			"chargeOverrides": [
         |				{
         |					"billCycleType": "DefaultFromCustomer",
         |					"billingPeriod": "Month",
         |					"endDateCondition": "Specific_End_Date",
         |					"specificEndDate": "$miniPaymentAdjustmentEndDate",
         |					"price": $formattedMiniPaymentAmount,
         |					"priceChangeOption": "NoChange",
         |					"productRatePlanChargeId": "${adjustment.productRatePlanChargeId}"
         |				}
         |			]
         |		},
         |		{
         |			"contractEffectiveDate": "$ongoingAdjustmentStartDate",
         |			"customerAcceptanceDate": "$ongoingAdjustmentStartDate",
         |			"serviceActivationDate": "$ongoingAdjustmentStartDate",
         |			"productRatePlanId": "${adjustment.productRatePlanId}",
         |			"chargeOverrides": [
         |				{
         |					"billCycleType": "DefaultFromCustomer",
         |					"billingPeriod": "Month",
         |					"endDateCondition": "Subscription_End",
         |					"price": $formattedOngoingAdjustmentAmount,
         |					"priceChangeOption": "NoChange",
         |					"productRatePlanChargeId": "${adjustment.productRatePlanChargeId}"
         |				}
         |			]
         |		}
         |	]
         |}"""
        .stripMargin

    val body = RequestBody.create(mediaType, json)

    val request = new Request.Builder()
      .url(s"https://rest.apisandbox.zuora.com/v1/subscriptions/${sub.subName}")
      .put(body)
      .addHeader("apiaccesskeyid", username)
      .addHeader("apisecretaccesskey", password)
      .addHeader("accept", "application/json")
      .addHeader("content-type", "application/json")
      .build()

    val response = client.newCall(request).execute()

    if (response.isSuccessful) {
      val bodyAsJson = Json.parse(response.body.string)
      bodyAsJson.validate[UpdateSubscriptionResponse] match {
        case success: JsSuccess[UpdateSubscriptionResponse] => {
          val updateSubResponse = success.get
          if (updateSubResponse.success) {
            logger.info(s"Successfully added discounts to sub: ${sub.subName}")
          } else {
            val error = s"Parsed Zuora response successfully, but success = false. Full body was: ${bodyAsJson}"
            logFailure(sub, error)
          }
        }
        case failure: JsError => {
          val error = s"Failed to convert Zuora response to case class - we got: ${bodyAsJson}"
          logFailure(sub, error)
        }
      }
    } else {
      val error = s"Response from Zuora was not successful, we got a ${response.code()}"
      logFailure(sub, error)
    }

    def logFailure(sub: Sub, error: String) = {
      logger.error(s"${sub.subName} FAILED to adjust sub. $error")
    }

  }

  adjustSub(UATADJUSTMENT)(
    Sub(
      subName = "A-S00070401",
      miniPaymentAmount = 1.46,
      oldAmount = 44.50,
      newAmount = 47.62,
      nextInvoiceDate = new DateTime(2018, 9, 5, 0, 0, 0, 0)
    )
  )

}
