import Refunder.{AccountForRefund, OutputCsvLine}
import com.typesafe.scalalogging.StrictLogging
import purecsv.unsafe._

object Reader extends StrictLogging {

  case class RefunderInput(accountId: String, overPaymentAmount: Double)

  def readForRefunder(filename: String): List[AccountForRefund] = {
    val rows = CSVReader[RefunderInput].readCSVFromFileName(filename, skipHeader = true)
    val accountsForRefund = rows.map { row =>
      AccountForRefund(
        accountId = row.accountId,
        totalOverpaymentAmount = row.overPaymentAmount
      )
    }
    logger.info(s"Read ${accountsForRefund.size} accounts...")
    accountsForRefund
  }

}

object Writer extends StrictLogging {

  def createOutputCsv(outputFileName: String, outputLines: List[OutputCsvLine]) = {
    logger.info(s"Writing ${outputLines.size} output lines to file")
    outputLines.writeCSVToFileName(fileName = outputFileName, header = Some(List("accountId", "negativeInvoiceNumber", "negativeInvoiceAmount", "invoiceNumberToCredit")))
  }

}
