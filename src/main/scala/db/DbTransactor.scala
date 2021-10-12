package db

import cats.effect.{Async, Blocker, ContextShift, Resource}
import db.DbConfig.{dbDriverName, dbPwd, dbUrl, dbUser}
import doobie.util.transactor.Transactor //other import

object DbTransactor {

  def make[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
    Blocker[F].map { be =>
      Transactor.fromDriverManager[F](
        driver  = dbDriverName,
        url     = dbUrl,
        user    = dbUser,
        pass    = dbPwd,
        blocker = be,
      )
    }

}
