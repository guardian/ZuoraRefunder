import Adjuster.Sub
import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import purecsv.unsafe._
import scala.util.Try

object Reader extends LazyLogging {

  case class Input(
                    subName: String,
                    miniPaymentAmount: String,
                    oldAmount: String,
                    newAmount: String,
                    nextInvoiceDate: String
                  )

  val dateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")

  def convertToCurrency(s: String): BigDecimal = BigDecimal(s.replaceAll("£", ""))

  def convertToDate(s: String): DateTime = dateTimeFormatter.parseDateTime(s)

  def read(filename: String): Try[List[Sub]] ={
    Try { //If we can't parse one row, we can't parse the file.
      val rows = CSVReader[Input].readCSVFromFileName(filename, skipHeader = true)
      rows.map { row =>
        logger.info(s"Reading subscription ${row.subName}")
        Sub(
          subName = row.subName,
          miniPaymentAmount = convertToCurrency(row.miniPaymentAmount),
          oldAmount = convertToCurrency(row.oldAmount),
          newAmount = convertToCurrency(row.newAmount),
          nextInvoiceDate = convertToDate(row.nextInvoiceDate)
        )
      }
    }
  }

}
