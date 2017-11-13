import java.time.LocalDate
import java.util.concurrent.TimeUnit
import okhttp3._
import play.api.libs.json._
import play.api.libs.json.Json.toJson
import play.api.libs.functional.syntax._
import scalaz.\/
import scalaz.Scalaz._

object ZuoraRefundService extends Logging {

  case class ZuoraRestConfig(username: String, password: String, baseUrl: String)

  val username = System.getenv("ZuoraApiAccessKeyId")
  val password = System.getenv("ZuoraApiSecretAccessKey")
  val baseUrl = System.getenv("ZuoraApiHost")

  val config = ZuoraRestConfig(username, password, baseUrl)

  val restClient = new OkHttpClient().newBuilder()
    .readTimeout(15, TimeUnit.SECONDS)
    .build()

  def buildRequest(config: ZuoraRestConfig, route: String): Request.Builder = {
    new Request.Builder()
      .addHeader("apiSecretAccessKey", config.password)
      .addHeader("apiAccessKeyId", config.username)
      .url(s"${config.baseUrl}/$route")
  }

  case class BasicAccountInfo(id: String, balance: Double)

  case class Subscription(subscriptionNumber: String, status: String) {
    def isActive: Boolean = status == "Active"
  }

  case class Invoice(id: String, invoiceNumber: String, amount: Double, balance: Double, status: String)

  case class PaidInvoice(invoiceId: String, invoiceNumber: String, appliedPaymentAmount: Double)

  case class Payment(id: String, status: String, effectiveDate: LocalDate, paidInvoices: List[PaidInvoice]) {
    def totalAmountPaid: Double = paidInvoices.map(invoice => invoice.appliedPaymentAmount).sum
  }

  case class AccountSummary(basicInfo: BasicAccountInfo, subscriptions: List[Subscription], invoices: List[Invoice], payments: List[Payment])

  case class RefundResult(success: Boolean, id: String)

  case class ProcessRefund(accountId: String, paymentId: String, refundAmount: Double)

  case class AccountUpdate(autoPay: Boolean)

  case class UpdateAccountResult(success: Boolean)

  implicit val basicAccountInfoReads: Reads[BasicAccountInfo] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "balance").read[Double]
  )(BasicAccountInfo.apply _)

  implicit val subscriptionReads: Reads[Subscription] = (
    (JsPath \ "subscriptionNumber").read[String] and
    (JsPath \ "status").read[String]
  )(Subscription.apply _)

  implicit val invoiceReads: Reads[Invoice] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "invoiceNumber").read[String] and
    (JsPath \ "amount").read[Double] and
    (JsPath \ "balance").read[Double] and
    (JsPath \ "status").read[String]
  )(Invoice.apply _)

  implicit val paidInvoiceReads: Reads[PaidInvoice] = (
      (JsPath \ "invoiceId").read[String] and
      (JsPath \ "invoiceNumber").read[String] and
      (JsPath \ "appliedPaymentAmount").read[Double]
    )(PaidInvoice.apply _)

  implicit val paymentReads: Reads[Payment] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "status").read[String] and
    (JsPath \ "effectiveDate").read[LocalDate] and
    (JsPath \ "paidInvoices").read[List[PaidInvoice]]
  )(Payment.apply _)

  implicit val accountSummaryReads: Reads[AccountSummary] = (
    (JsPath \ "basicInfo").read[BasicAccountInfo] and
    (JsPath \ "subscriptions").read[List[Subscription]] and
    (JsPath \ "invoices").read[List[Invoice]] and
    (JsPath \ "payments").read[List[Payment]]
  )(AccountSummary.apply _)

  implicit val refundResult: Reads[RefundResult] = (
    (JsPath \ "Success").read[Boolean] and
    (JsPath \ "Id").read[String]
  )(RefundResult.apply _)

  implicit val processRefundWrites = new Writes[ProcessRefund] {
    def writes(processRefund: ProcessRefund) = Json.obj(
      "AccountId" -> processRefund.accountId,
      "Amount" -> processRefund.refundAmount,
      "PaymentId" -> processRefund.paymentId,
      "ReasonCode" -> "Customer Satisfaction",
      "SourceType" -> "Payment",
      "Type" -> "Electronic"
    )
  }

  implicit val updateAccountResultReads: Reads[UpdateAccountResult] = (JsPath \ "success").read[Boolean].map {
    success => UpdateAccountResult(success)
  }

  implicit val accountUpdateWrites = new Writes[AccountUpdate] {
    def writes(accountUpdate: AccountUpdate) = Json.obj(
      "autoPay" -> accountUpdate.autoPay
    )
  }

  def convertResponseToCaseClass[T](accountId: String, response: Response)(implicit r: Reads[T]): String \/ T = {
    if (response.isSuccessful) {
      val bodyAsJson = Json.parse(response.body.string)
      bodyAsJson.validate[T] match {
        case success: JsSuccess[T] => success.get.right
        case error: JsError => {
          logFailureResult(accountId, s"failed to convert Zuora response to case case. Response body was: \n ${bodyAsJson}")
          "Error when converting Zuora response to case class".left
        }
      }
    } else {
      logFailureResult(accountId, s"request to Zuora was unsuccessful, the response was: \n $response | body was: \n ${response.body.string}")
      "Request to Zuora was unsuccessful".left
    }
  }

  def getAccountSummary(accountId: String): String \/ AccountSummary = {
    logInfo(accountId, s"getting account summary from Zuora")
    val request = buildRequest(config, s"accounts/$accountId/summary").get().build()
    val call = restClient.newCall(request)
    val response = call.execute
    convertResponseToCaseClass[AccountSummary](accountId, response)
  }

  def disableAutoPay(accountId: String): String \/ UpdateAccountResult = {
    val accountUpdate = AccountUpdate(autoPay = false)
    val body = RequestBody.create(MediaType.parse("application/json"), Json.toJson(accountUpdate).toString)
    val request = buildRequest(config, s"accounts/${accountId}").put(body).build()
    val call = restClient.newCall(request)
    logger.info(s"Attempting to disable autoPay with the following command: $accountUpdate")
    val response = call.execute
    convertResponseToCaseClass[UpdateAccountResult](accountId, response)
  }

  def processRefund(accountId: String, paymentId: String, refundAmount: Double): String \/ RefundResult = {
    val processRefund = ProcessRefund(accountId, paymentId, refundAmount)
    val json = toJson(processRefund)
    val body = RequestBody.create(MediaType.parse("application/json; charset=utf-8"), json.toString)
    val request = buildRequest(config, s"object/refund").post(body).build()
    val call = restClient.newCall(request)
    logInfo(accountId, s"processing refund in Zuora: $json")
    val response = call.execute
    convertResponseToCaseClass[RefundResult](accountId, response)
  }

}
