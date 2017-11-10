import java.time.LocalDate
import ZuoraRefundService.{AccountSummary, Invoice, Payment}
import scalaz.{\/, \/-}
import scalaz.Scalaz._

object Refunder extends App with Logging {

  case class AccountForRefund(accountId: String, totalOverpaymentAmount: Double)

  case class OutputCsvLine(accountId: String, negativeInvoiceId: String, negativeInvoiceAmount: Double, invoiceToCreditId: String)

  def identifyValidPayment(accountSummary: AccountSummary, totalOverpaymentAmount: Double): String \/ Payment = {
    val cutOffDate = LocalDate.of(2017, 8, 13)
    val payments = accountSummary.payments
    val validPayments = payments.filter(payment => payment.effectiveDate.isAfter(cutOffDate) && payment.status == "Processed" && payment.paidInvoices.head.appliedPaymentAmount > totalOverpaymentAmount)
    if (validPayments.size == 1) {
      val validPayment = validPayments.head
      logInfo(accountSummary.basicInfo.id, s"Found valid payment for refund invoice: $validPayment")
      validPayment.right
    } else {
      logFailureResult(accountSummary.basicInfo.id, s"Failed to identify valid payment for refund, we got: $payments")
      s"Failed to identify valid payment for refund, we got: $payments".left
    }
  }

  def matchingNegativeInvoice(accountSummary: AccountSummary, totalOverpaymentAmount: Double): String \/ Invoice = {
    val negativeInvoices = accountSummary.invoices.filter(invoice => invoice.balance < 0)
    if (negativeInvoices.size == 1) {
      val negativeInvoice = negativeInvoices.head
      if (negativeInvoice.balance + totalOverpaymentAmount == 0) {
        negativeInvoice.right
      } else {
        logFailureResult(accountSummary.basicInfo.id, "Could not find a negative invoice of the expected value")
        "Could not find a negative invoice of the expected value".left
      }
    } else {
      logFailureResult(accountSummary.basicInfo.id, "Could not find a negative invoice or found too many negative invoices")
      "Could not find a negative invoice or found too many negative invoices".left
    }
  }

  def processRefund(accountSummary: AccountSummary, payment: Payment, totalOverpaymentAmount: Double, negativeInvoice: Invoice): String \/ Unit = {
    val accountId = accountSummary.basicInfo.id
    ZuoraRefundService.processRefund(accountId, payment.id, totalOverpaymentAmount).map { refundResult =>
      if (refundResult.success) {
        logSuccessfulResult(accountId, refundResult.id)
      } else {
        logFailureResult(accountId, s"failed to process refund")
      }
    }
  }

  def produceOutputLine(accountSummary: AccountSummary, negativeInvoice: Invoice, invoiceNumberToCredit: String): OutputCsvLine = {
    val line = OutputCsvLine(accountSummary.basicInfo.id, negativeInvoice.invoiceNumber, negativeInvoice.amount, invoiceNumberToCredit)
    logInfo(accountSummary.basicInfo.id, s"Produced output csv line: $line")
    line
  }

  logger.info("Refunder is starting...")
  val inputFileName = "input.csv"
  val outputFileName = "output.csv"
  val accounts = Reader.readForRefunder(inputFileName)

  val outputLines = accounts.map { account =>

    val outputCsvLine = for {
      accountSummary <- ZuoraRefundService.getAccountSummary(account.accountId)
      validPayment <- identifyValidPayment(accountSummary, account.totalOverpaymentAmount)
      negativeInvoice <- matchingNegativeInvoice(accountSummary, account.totalOverpaymentAmount)
      _ <- processRefund(accountSummary, validPayment, account.totalOverpaymentAmount, negativeInvoice)
      output = produceOutputLine(accountSummary, negativeInvoice, validPayment.paidInvoices.head.invoiceNumber)
    } yield output

    outputCsvLine

  }

  val successfulRefunds = outputLines.collect { case \/-(line) => line }

  Writer.createOutputCsv(outputFileName, successfulRefunds)

}
