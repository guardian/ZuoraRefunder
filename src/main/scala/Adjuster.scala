import okhttp3._
import com.github.nscala_time.time.Imports._
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.util.{Failure, Success}

object Adjuster extends App with LazyLogging {

  val username = System.getenv("Z_USER")
  val password = System.getenv("Z_PASS")

  case class Sub(
                  subName: String,
                  miniOverpaymentAmount: Option[BigDecimal],
                  oldAmount: BigDecimal,
                  newAmount: BigDecimal,
                  nextInvoiceDate: DateTime
                )

  implicit val updateSubscriptionReads: Reads[UpdateSubscriptionResponse] = (
    (JsPath \ "success").read[Boolean] and
      (JsPath \ "subscriptionId").read[String]
    ) (UpdateSubscriptionResponse.apply _)

  case class UpdateSubscriptionResponse(
                                         success: Boolean,
                                         subscriptionId: String
                                       )

  case class ZuoraCatalogIds(
                              productRatePlanId: String,
                              productRatePlanChargeId: String
                            )

  case class OngoingAdjustmentData(
                                    ongoingAdjStart: String,
                                    ongoingAdjAmount: String)

  case class MiniAdjustmentData(
                                 miniAdjStart: String,
                                 miniAdjEnd: String,
                                 miniAdjAmount: String)

  case class AdjustmentData(
                             subName: String,
                             maybeOngoingAdjustment: Option[OngoingAdjustmentData],
                             maybeMiniAdjustment: Option[MiniAdjustmentData]
                           )

  val client = new OkHttpClient()

  val mediaType = MediaType.parse("application/json")

  def logFailure(subName: String, error: String) = {
    logger.error(s"${subName}: FAILED to adjust sub. $error")
  }

  def prepareAdjustmentData(sub: Sub): AdjustmentData = {

    logger.info(s"${sub.subName}: Preparing adjustment data...")

    def dateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")

    def formatDate(d: DateTime): String = dateFormatter.print(d)

    def formatCurrency(m: BigDecimal): String = m.setScale(2, BigDecimal.RoundingMode.HALF_UP).toString

    def formatDiscount(m: BigDecimal): String = s"-${formatCurrency(m)}"

    val monthlyOverpayment = sub.newAmount - sub.oldAmount

    val ongoingAdjustment = if (monthlyOverpayment >= 0.01) {
      val ongoingAdjustmentStartDate = formatDate(sub.nextInvoiceDate)
      Some(OngoingAdjustmentData(ongoingAdjustmentStartDate, formatDiscount(monthlyOverpayment)))
    } else {
      None
    }

    val miniOverpaymentAdjustment = sub.miniOverpaymentAmount.map { amount =>
      val formattedMiniPaymentAmount = formatDiscount(amount)
      val miniPaymentAdjustmentStartDate = formatDate(sub.nextInvoiceDate.minusMonths(1))
      val miniPaymentAdjustmentEndDate = formatDate(sub.nextInvoiceDate) //Exclusive end date not inclusive
      MiniAdjustmentData(miniPaymentAdjustmentStartDate, miniPaymentAdjustmentEndDate, formattedMiniPaymentAmount)
    }

    val adjustmentData = AdjustmentData(
      sub.subName,
      ongoingAdjustment,
      miniOverpaymentAdjustment
    )

    logger.info(s"${sub.subName}: adjustment data is: $adjustmentData}")
    adjustmentData

  }

  def adjustSubInZuora(catalogIds: ZuoraCatalogIds, adjustmentData: AdjustmentData): Unit = {

    val subName = adjustmentData.subName

    val json = generateJson(catalogIds, adjustmentData)

    val body = RequestBody.create(mediaType, json)

    val request = new Request.Builder()
      .url(s"https://rest.apisandbox.zuora.com/v1/subscriptions/${subName}")
      .put(body)
      .addHeader("apiaccesskeyid", username)
      .addHeader("apisecretaccesskey", password)
      .addHeader("accept", "application/json")
      .addHeader("content-type", "application/json")
      .build()

    logger.info(s"s${adjustmentData.subName}: attempting to adjust sub in Zuora")
    val response = client.newCall(request).execute()

    if (response.isSuccessful) {
      val bodyAsJson = Json.parse(response.body.string)
      bodyAsJson.validate[UpdateSubscriptionResponse]
      match {
        case success: JsSuccess[UpdateSubscriptionResponse] => {
          val updateSubResponse = success.get
          if (updateSubResponse.success) {
            logger.info(s"${subName}: Successfully added discounts to sub")
          } else {
            val error = s"Parsed Zuora response successfully, but success = false. Full body was: ${bodyAsJson}"
            logFailure(subName, error)
          }
        }
        case failure: JsError => {
          val error = s"Failed to convert Zuora response to case class - we got: ${bodyAsJson}"
          logFailure(subName, error)
        }
      }
    } else {
      val error = s"Response from Zuora was not successful, we got a ${response.code()}"
      logFailure(subName, error)
    }

  }

  def generateJson(catalogIds: ZuoraCatalogIds, adjustmentData: AdjustmentData): String = {

    val miniPaymentDiscountJson = adjustmentData.maybeMiniAdjustment.map{miniAdjustmentData =>
      s"""
         |{
         |	"contractEffectiveDate": "${miniAdjustmentData.miniAdjStart}",
         |	"customerAcceptanceDate": "${miniAdjustmentData.miniAdjStart}",
         |	"serviceActivationDate": "${miniAdjustmentData.miniAdjStart}",
         |	"productRatePlanId": "${catalogIds.productRatePlanId}",
         |	"chargeOverrides": [
         |		{
         |			"billCycleType": "DefaultFromCustomer",
         |			"billingPeriod": "Month",
         |			"endDateCondition": "Specific_End_Date",
         |			"specificEndDate": "${miniAdjustmentData.miniAdjEnd}",
         |			"price": ${miniAdjustmentData.miniAdjAmount},
         |			"priceChangeOption": "NoChange",
         |			"productRatePlanChargeId": "${catalogIds.productRatePlanChargeId}"
         |		}
         |	]
         |}""".stripMargin
    }



    val ongoingPaymentDiscountJson = adjustmentData.maybeOngoingAdjustment.map { ongoingAdjustmentData =>

          s"""
             |{
             |	"contractEffectiveDate": "${ongoingAdjustmentData.ongoingAdjStart}",
             |	"customerAcceptanceDate": "${ongoingAdjustmentData.ongoingAdjStart}",
             |	"serviceActivationDate": "${ongoingAdjustmentData.ongoingAdjStart}",
             |	"productRatePlanId": "${catalogIds.productRatePlanId}",
             |	"chargeOverrides": [
             |		{
             |			"billCycleType": "DefaultFromCustomer",
             |			"billingPeriod": "Month",
             |			"endDateCondition": "Subscription_End",
             |			"price": ${ongoingAdjustmentData.ongoingAdjAmount},
             |			"priceChangeOption": "NoChange",
             |			"productRatePlanChargeId": "${catalogIds.productRatePlanChargeId}"
             |		}
             |	]
             |}""".stripMargin


    }

    val adjustments = Seq(miniPaymentDiscountJson, ongoingPaymentDiscountJson).flatten

    val json =
      s"""
         |{
         |	"add": [
         |	      ${adjustments.mkString(",")}
         |	]
         |}""".stripMargin

    logger.info(s"${adjustmentData.subName}: JSON will be: $json")
    json

  }

  val uatIds = ZuoraCatalogIds(productRatePlanId = "2c92c0f85e0d9c02015e0e527a5e7120", productRatePlanChargeId = "2c92c0f95e0da917015e0e54be576690")

  logger.info("Starting Adjuster script... attempting to read CSV")

  val tryToRead = Reader.read("test.csv")

  tryToRead match {
    case Success(subs) => {
      logger.info(s"Successfully read file: $subs")
      logger.info("Starting to process data for adjustments")
      val adjustments = subs.map(sub => prepareAdjustmentData(sub))
      logger.info("Finished preparing adjustment data; starting processing...")
      adjustments.map(adjustment => generateJson(uatIds, adjustment))
      // call zuora
    }
    case Failure(ex) => {
      logger.error(s"Couldn't read file due to: $ex")
    }
  }

}
