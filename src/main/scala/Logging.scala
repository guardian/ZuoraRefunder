import com.typesafe.scalalogging.StrictLogging

trait Logging extends StrictLogging {

  def logInfo(accountId: String, message: String): Unit = {
    logger.info(s"AccountId $accountId: $message")
  }

  def logError(accountId: String, message: String): Unit = {
    logger.error(s"AccountId $accountId: $message")
  }

  def logSuccessfulResult(accountId: String, refundIds: List[String]): Unit = {
    logInfo(accountId, s"SUCCESSFUL refund processing. RefundIds: $refundIds")
  }

  def logFailureResult(accountId: String, errorMessage: String): Unit = {
    logError(accountId, s"FAILURE during refund processing: $errorMessage")
  }

}
