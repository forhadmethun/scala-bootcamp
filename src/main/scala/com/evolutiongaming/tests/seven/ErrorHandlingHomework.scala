package com.evolutiongaming.tests.seven

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all._
import com.evolutiongaming.tests.seven.ErrorHandlingHomework.ValidationError._

import java.time.YearMonth

/** Implement all methods marked with `???` below according to their Scaladoc description.
  * Add helper methods and more unit tests in a separate file if necessary to properly verify the solution.
  */
object ErrorHandlingHomework {

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  /** Payment card number. */
  final case class Number(value: String) extends AnyVal
  object Number {

    /** Constructs [[Number]] from the string.
      * <p/>
      * The string must contain between 8 and 19 digits (inclusive) and must not start with 0.
      */
    def fromString(s: String): AllErrorsOr[Number] = {

      def startsWithZero: AllErrorsOr[Number] = if (s.headOption.exists(_.equals('0'))) NumberStartsWithZero.invalidNec else Number(s).validNec

      def validCharacters: AllErrorsOr[Number] = if (!s.forall(_.isDigit)) NumberContainsInvalidCharacters.invalidNec else Number(s).validNec

      def validLength: AllErrorsOr[Number] = if (s.length < 8 || s.length > 19) NumberIsOutOfRange.invalidNec else Number(s).validNec

      startsWithZero.productR(validCharacters).productR(validLength)
    }
  }

  /** Payment card expiration date. */
  final case class ExpirationDate(value: YearMonth) extends AnyVal
  object ExpirationDate {

    /** Constructs [[ExpirationDate]] from the string.
      * <p/>
      * The string must be in the format `MM/YYYY`. `MM` part must be a pair of digits from "01" to "12".
      * `YYYY` part must be a pair of digits from "0001" to "9999". For example, `02/2024` means the card
      * is active until the last day of February 2024.
      * <p/>
      * [[ExpirationDate]] must not be in the past, i.e. before the current [[YearMonth]], as supplied
      * by the `now` function.
      *
      * @param now the function that returns the current [[YearMonth]]
      */
    def fromString(s: String)(now: () => YearMonth): AllErrorsOr[ExpirationDate] = {
      val parts = s.split("/")
      if (parts.length != 2) {
        ValidationError.ExpirationDateHasWrongFormat.invalidNec
      } else {
        val monthStr = parts(0)
        val yearStr = parts(1)
        val month = monthStr.toIntOption
        val year = yearStr.toIntOption
        val validMonth = month.exists(m => m >= 1 && m <= 12)
        val validYear = year.exists(y => y >= 1 && y <= 9999)
        if (!validMonth || !validYear) {
          ValidationError.ExpirationDateHasWrongFormat.invalidNec
        } else {
          val expirationDate = YearMonth.of(year.get, month.get)
          if (expirationDate.isBefore(now())) {
            ValidationError.ExpirationDateIsInThePast.invalidNec
          } else {
            ExpirationDate(expirationDate).validNec
          }
        }
      }
    }
  }

  /** Payment card holder name. */
  final case class Name(value: String) extends AnyVal
  object Name {

    /** Constructs [[Name]] from the string.
      * <p/>
      * The string must be between 2 and 26 characters (inclusive). It must contain only spaces, upper and
      * lower case letters. It must not start or end with a space.
      */
    def fromString(s: String): AllErrorsOr[Name] = {

      def startsAndEndsWithSpace: AllErrorsOr[Name] = if (s.headOption.exists(_.equals(' ')) || s.endsWith(" ")) NameStartsOrEndsWithSpace.invalidNec else Name(s).validNec

      def validCharacters: AllErrorsOr[Name] = if (!s.matches("^[_a-zA-Z0-9 ]+$")) NameContainsInvalidCharacters.invalidNec else Name(s).validNec

      def validLength: AllErrorsOr[Name] = if (!(2 to 26 contains s.length)) NameIsOutOfRange.invalidNec else Name(s).validNec

      startsAndEndsWithSpace.productR(validCharacters).productR(validLength)
    }
  }

  /** Payment card security code. */
  final case class SecurityCode(value: String) extends AnyVal
  object SecurityCode {

    /** Constructs [[SecurityCode]] from the string.
      * <p/>
      * The string must contain between 3 and 4 digits (inclusive).
      */
    def fromString(s: String): AllErrorsOr[SecurityCode] = {
      Validated.condNec(
        s.matches("""^\d{3,4}$"""),
        SecurityCode(s),
        SecurityCodeIsInvalid
      )
    }
  }

  /** Payment card. */
  final case class PaymentCard(
    number: Number,
    expirationDate: ExpirationDate,
    name: Name,
    securityCode: SecurityCode,
  )

  /** Payment card validation error. */
  sealed trait ValidationError
  object ValidationError {

    /** Payment card number contains less than 8 or more than 19 characters. */
    final case object NumberIsOutOfRange extends ValidationError

    /** Payment card number contains characters other than digits. */
    final case object NumberContainsInvalidCharacters extends ValidationError

    /** Payment card number starts with 0. */
    final case object NumberStartsWithZero extends ValidationError

    /** Expiration date is not in the format "MM/YYYY". */
    final case object ExpirationDateHasWrongFormat extends ValidationError

    /** Expiration date is in the past, i.e. earlier than the current month of the current year. */
    final case object ExpirationDateIsInThePast extends ValidationError

    /** Payment card name contains less than 2 or more than 26 characters. */
    final case object NameIsOutOfRange extends ValidationError

    /** Payment card name starts or ends with a space. */
    final case object NameStartsOrEndsWithSpace extends ValidationError

    /** Payment card name contains characters other than spaces, upper and lower case letters. */
    final case object NameContainsInvalidCharacters extends ValidationError

    /** Payment card security code is invalid. */
    final case object SecurityCodeIsInvalid extends ValidationError
  }

  object PaymentCardValidator {

    /** Attempts to construct [[PaymentCard]] from the supplied raw strings. Aggregates all encountered
      * validation errors. Returns as many errors as possible for any given set of input parameters.
      * <p/>
      * For example, if the supplied number is "0ABC", while the security code is "DEF", the returned errors
      * must include [[NumberIsOutOfRange]], [[NumberContainsInvalidCharacters]], [[NumberStartsWithZero]]
      * and [[SecurityCodeIsInvalid]].
      *
      * @param now the function that returns the current [[YearMonth]]
      *
      * @return [[PaymentCard]] or all encountered validation errors.
      */
    def validate(
      number: String,
      expirationDate: String,
      name: String,
      securityCode: String,
    )(now: () => YearMonth): AllErrorsOr[PaymentCard] =
      (Number.fromString(number), ExpirationDate.fromString(expirationDate)(now), Name.fromString(name), SecurityCode.fromString(securityCode)).mapN(PaymentCard)
  }
}
