import java.time.LocalDate
import ZuoraRefundService.{AccountSummary, Invoice, Payment}
import scalaz.{-\/, \/, \/-}
import scalaz.Scalaz._

object Refunder extends App with Logging {

  case class AccountForRefund(accountId: String, totalOverpaymentAmount: Double)

  case class OutputCsvLine(accountId: String, negativeInvoiceId: String, negativeInvoiceAmount: Double)

  def identifyEligiblePayments(accountSummary: AccountSummary, totalOverpaymentAmount: Double): String \/ List[Payment] = {
    val cutOffDate = LocalDate.of(2017, 10, 31)
    val eligiblePayments = accountSummary.payments.filter(payment => payment.effectiveDate.isAfter(cutOffDate) && payment.status == "Processed")
    val recentPaymentsTotalValue = eligiblePayments.map(_.totalAmountPaid).sum
    logInfo(accountSummary.basicInfo.id, s"comparing calculated refund with identified payments. From csv: $totalOverpaymentAmount | from account summary: $recentPaymentsTotalValue")
    if (recentPaymentsTotalValue == totalOverpaymentAmount) {
      logInfo(accountSummary.basicInfo.id, s"Found valid payments for refund: $eligiblePayments")
      eligiblePayments.right
    } else {
      logFailureResult(accountSummary.basicInfo.id, s"Failed to identify valid payments for refund, we got: $eligiblePayments")
      s"Failed to identify valid payment for refund, we got: $eligiblePayments".left
    }
  }

  def processRefunds(accountSummary: AccountSummary, payments: List[Payment]): String \/ Unit = {
    val accountId = accountSummary.basicInfo.id
    val refundAttempts = payments.map {
      payment => ZuoraRefundService.processRefund(accountId, payment.id, payment.totalAmountPaid)
    }
    val failures = refundAttempts.collect { case -\/(error) => error }
    val successes = refundAttempts.collect { case \/-(refundResult) => refundResult }
    if (failures.isEmpty) {
      logSuccessfulResult(accountId, successes.map(_.id))
      (()).right
    } else {
      logFailureResult(accountId, s"failed to process refunds")
      failures.toString.left
    }
  }

  def produceOutputLine(accountSummary: AccountSummary, negativeInvoice: Invoice): OutputCsvLine = {
    val line = OutputCsvLine(accountSummary.basicInfo.id, negativeInvoice.invoiceNumber, negativeInvoice.amount)
    logInfo(accountSummary.basicInfo.id, s"Produced output csv line: $line")
    line
  }

  def filterAccounts(accountSummary: AccountSummary): String \/ Unit = {
    if (accountSummary.subscriptions.exists(_.isActive)) {
      val failureReason = s"Account has an active subscription"
      logFailureResult(accountSummary.basicInfo.id, failureReason)
      failureReason.left
    } else {
      logInfo(accountSummary.basicInfo.id, s"All subscriptions on account are inactive")
      (()).right
    }
  }

  logger.info("Refunder is starting...")
  val inputFileName = "test.csv"
  val outputFileName = "output.csv"
  val accounts = Reader.readForRefunder(inputFileName)

  val outputLines = accounts.map { account =>

    val allRefunds = for {
      accountSummary <- ZuoraRefundService.getAccountSummary(account.accountId)
      filter <- filterAccounts(accountSummary)
      disableAutoPay <- ZuoraRefundService.disableAutoPay(account.accountId)
      eligiblePayments <- identifyEligiblePayments(accountSummary, account.totalOverpaymentAmount)
      refunds <- processRefunds(accountSummary, eligiblePayments)
    } yield refunds

    allRefunds

  }

  val successfulRefunds = outputLines.collect { case \/-(line) => line }

  logger.info(s"Successfully processed refunds for ${successfulRefunds.size} accounts")

}
