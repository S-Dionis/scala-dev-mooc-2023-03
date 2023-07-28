package catsconcurrency.cats_effect_homework

import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import Wallet._

import java.nio.charset.StandardCharsets
import java.nio.file.Files._
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  def balance: F[BigDecimal] = for {
    exists <- Sync[F].blocking(Files.exists(Paths.get(id)))
    balance <-
      if (exists) {
        Sync[F].blocking(BigDecimal(Files.readString(Paths.get(id)))).handleErrorWith(_ => Sync[F].pure(BigDecimal(0)))
      } else {
        Sync[F].raiseError(new Throwable("Id file not found"))
      }
  } yield balance

  def topup(amount: BigDecimal): F[Unit] = for {
    b <- balance
    _ <- Sync[F].blocking {
      Files.write(
        Paths.get(id),
        (b + amount).toString.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }
  } yield Sync[F].unit

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
    for {
      b <- balance
      r <- if (b >= amount) {
          topup(-amount).map(_.asRight)
        } else {
          Sync[F].blocking(BalanceTooLow.asLeft)
        }
    } yield r

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =
    Sync[F].delay(Files.deleteIfExists(Paths.get(id))) *>
      Sync[F].delay(Files.createFile(Paths.get(id))) *>
      Sync[F].delay(new FileWallet(id))

  type WalletId = String

  sealed trait WalletError  extends Throwable
  case object BalanceTooLow extends WalletError
}
