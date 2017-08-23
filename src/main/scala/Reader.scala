import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import purecsv.unsafe._

import scala.util.{Success, Try}
object Reader extends LazyLogging{
  case class Input(
                    subName: String,
                    miniPaymentAmount: String,
                    oldAmount: String,
                    newAmount: String,
                    nextInvoiceDate: String
                  )
  val dateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")

  def convertToCurrency(s: String):BigDecimal = BigDecimal(s.replaceAll("£",""))
  def convertToDate(s: String): DateTime = dateTimeFormatter.parseDateTime(s)
  def read(filename: String): Try[List[Adjuster.Sub]] ={
    Try { //If we can't parse one row, we can't parse the file.
      val rows = CSVReader[Input].readCSVFromFileName("to-adjust.csv")
      rows.map { row =>
        logger.info(s"Reading subscription ${row.subName}")
        Adjuster.Sub(
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
