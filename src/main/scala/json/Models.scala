package json

object Models {

  final case class Post(
    userId: Int,
    id:     Int,
    title:  String,
    body:   String
  )

  final case class Comment(
    postId: Int,
    id:     Int,
    name:   String,
    email:  String,
    body:   String
  )

  final case class Album(
    userId: Int,
    id:     Int,
    title:  String
  )

  final case class Todo(
    userId:    Int,
    id:        Int,
    title:     String,
    completed: Boolean
  )

  final case class User(
    id:       Int,
    name:     String,
    username: String,
    email:    String,
    address:  Address,
    phone:    String,
    website:  String,
    company:  Company
  )

  final case class Address(
    street:  String,
    suite:   String,
    city:    String,
    zipcode: String,
    geo:     Geo
  )

  final case class Geo(
    lat: String,
    lng: String
  )

  final case class Company(
    name:        String,
    catchPhrase: String,
    bs:          String
  )

}
