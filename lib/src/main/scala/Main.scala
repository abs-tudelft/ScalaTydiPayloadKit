import io.circe.generic.auto._
import io.circe.parser._
import scala.io.Source
import scala.util.Using

object Main extends App {

  // Case classes to represent the JSON data structure
  case class Author(userId: Int, username: String)
  case class Comment(commentId: Int, author: Author, content: String, createdAt: String, likes: Int, inReplyToCommentId: Option[Int])
  case class Post(
                   postId: Int,
                   title: String,
                   content: String,
                   author: Author,
                   createdAt: String,
                   updatedAt: String,
                   tags: List[String],
                   likes: Int,
                   shares: Int,
                   comments: List[Comment]
                 )

  /**
   * Reads the content of a file into a single string.
   *
   * @param filename The path to the file.
   * @return An `Either` containing the file content as a `String` on success,
   * or a `Throwable` on failure.
   */
  def readJsonFromFile(filename: String): Either[Throwable, String] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().mkString
    }.toEither
  }

  /**
   * Prints the details of a list of posts in a human-readable format.
   *
   * @param posts The list of `Post` objects to print.
   */
  def printPosts(posts: List[Post]): Unit = {
    println("Successfully parsed and processed the JSON data:")
    posts.foreach(post => {
      println(s"Post ID: ${post.postId}, Title: ${post.title}, Likes: ${post.likes}")
      println(s"  Author: ${post.author.username} (ID: ${post.author.userId})")
      println(s"  Comments (${post.comments.length}):")
      post.comments.foreach(comment => {
        println(s"    - Comment ID: ${comment.commentId}, Author: ${comment.author.username}, Likes: ${comment.likes}")
        comment.inReplyToCommentId.foreach(inReplyToId =>
          println(s"    - (In reply to comment ID: $inReplyToId)")
        )
      })
      println("-" * 20)
    })
  }

  val filename = "posts.json"

  // Read the JSON file and then parse the content.
  // The result is an `Either[io.circe.Error, List[Post]]`.
  val parsedPosts: Either[io.circe.Error, List[Post]] = for {
    jsonString <- readJsonFromFile(filename).left.map(t => io.circe.ParsingFailure(s"Failed to read file: ${t.getMessage}", t))
    posts <- decode[List[Post]](jsonString)
  } yield posts

  // Use a `match` expression to extract the `posts` value or exit the application.
  val posts: List[Post] = parsedPosts match {
    case Right(validPosts) => validPosts
    case Left(error) =>
      println(s"Failed to parse JSON: $error")
      sys.exit(1)
  }

  // All subsequent processing can now be done on the 'posts' variable,
  // knowing that it's a valid List[Post].
  printPosts(posts)

  val root_stream = TydiStream.from_seq(posts)
  val title_stream = root_stream.drill(_.title)
  val tags_stream = root_stream.drill(_.tags).drill(x => x)
  val comments_stream = root_stream.drill(_.comments)
  println(title_stream)
  println(tags_stream)
  println(comments_stream)
}
