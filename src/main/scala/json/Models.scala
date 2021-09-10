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

}
