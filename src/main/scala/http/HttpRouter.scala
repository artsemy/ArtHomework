package http

import cats.implicits._
import cats.effect.Sync
import org.http4s.{EntityEncoder, HttpRoutes, Response}
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.dsl.Http4sDsl
import tf.validation.EmployeeValidator.EmployeeValidationError
import tf.validation.EmployeeValidator.EmployeeValidationError._
import tf.services.EmployeeService
import tf.domain.employee.{CreateEmployeeDTO, EmployeeDTO}

object HttpRouter {

  def routes[F[_]: Sync](service: EmployeeService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def all: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "employee" / "all" =>
      for {
        res      <- service.all
        response <- Ok(res.toString())
      } yield response
    }

    def create: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "employee" / "create" =>
      req.as[CreateEmployeeDTO].flatMap { dto =>
        val empDto = EmployeeDTO("id", dto.birthday, dto.firstName, dto.lastName, dto.salary, dto.position)
        val res    = service.create(empDto)
        marshalResponse(res)
      }
    }

    def update: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "employee" / "update" =>
      req.as[EmployeeDTO].flatMap { dto =>
        val res = service.update(dto)
        marshalResponse(res)
      }
    }

    def find: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "employee" / "find" / id =>
      val res = service.find(id).map(x => x.toRight[EmployeeValidationError](EmployeeNotFound))
      marshalResponse(res)
    }

    def delete: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root / "employee" / "delete" / id =>
      val res = service.delete(id).map(x => Option(x).toRight[EmployeeValidationError](EmployeeNotFound))
      marshalResponse(res)
    }

    def marshalResponse[T](
      result: F[Either[EmployeeValidationError, T]]
    )(
      implicit E: EntityEncoder[F, T]
    ): F[Response[F]] =
      result
        .flatMap {
          case Left(_)    => BadRequest(EmployeeHttpFormat.toString) //fix???
          case Right(dto) => Ok(dto.toString)
        }
        .handleErrorWith { ex =>
          InternalServerError(ex.getMessage)
        }

    all <+> create <+> update <+> find <+> delete
  }
}
