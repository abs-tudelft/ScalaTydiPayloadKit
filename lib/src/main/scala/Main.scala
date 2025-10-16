package TydiPackaging

import io.circe.generic.auto._
import io.circe.parser._
import java.time.Instant
import scala.io.Source
import scala.util.Using
// Even though the implicit conversion definitions are not actively used, they *need* to be imported to be available for the compiler.
import CustomBinaryConversions._

object Main extends App {

  // Case classes to represent the JSON data structure
  case class Author(userId: Int, username: String)
  case class Comment(commentId: Int, author: Author, content: String, createdAt: Instant, likes: Int, inReplyToCommentId: Option[Int])
  case class Post(
                   postId: Int,
                   title: String,
                   content: String,
                   author: Author,
                   createdAt: Instant,
                   updatedAt: Instant,
                   tags: List[String],
                   likes: Int,
                   shares: Int,
                   comments: List[Comment]
                 )

  case class PhysicalStreamsTyped(
                                  posts: TydiStream[Post],
                                  post_titles: TydiStream[Char],
                                  post_contents: TydiStream[Char],
                                  post_author_username: TydiStream[Char],
                                  post_tags: TydiStream[Char],
                                  post_comment_author_username: TydiStream[Char],
                                  post_comment_content: TydiStream[Char],
                                  post_comments: TydiStream[Comment]
                                 ) {
    def reverse(): Seq[Post] = {
      val comments_recreated = post_comments
//      comments_recreated.inject_string(|el| &mut el.author.username, self.post_comment_author_username);
//      comments_recreated.inject_string(|el| &mut el.content, self.post_comment_content);

      val posts_recreated = posts.inject[Comment]((p: Post, s) => p.copy(comments = s.toList), comments_recreated)
//      val tags_recreated = post_tags.solidify_into_strings()
//      posts_recreated.inject(|el| &mut el.tags, tags_recreated)
//      posts_recreated.inject_string(|el| &mut el.title, self.post_titles)
//      posts_recreated.inject_string(|el| &mut el.content, self.post_contents)
//      posts_recreated.inject_string(|el| &mut el.author.username, self.post_author_username)

      posts_recreated.toSeq
    }
  }

  object PhysicalStreamsTyped {
    def apply(posts: Seq[Post]): PhysicalStreamsTyped = {
      val posts_tydi = TydiStream.fromSeq(posts);
      val titles_tydi = posts_tydi.drill(_.title);
      val contents_tydi = posts_tydi.drill(_.content);
      val tags_tydi = posts_tydi.drill(_.tags).drill(x => x);
      val comments_tydi = posts_tydi.drill(_.comments);
      val comment_author_tydi = comments_tydi.drill(_.author.username);

      new PhysicalStreamsTyped(
        post_titles=titles_tydi,
        post_contents=contents_tydi,
        post_author_username=posts_tydi.drill(_.author.username),
        post_tags=tags_tydi,
        post_comment_author_username=comment_author_tydi,
        post_comment_content=comments_tydi.drill(_.content),
        post_comments=comments_tydi,
        posts=posts_tydi
      )
    }
  }

  type TydiBinaryStream = Seq[TydiBinary]

  case class PhysicalStreamsBinary(
                                    posts: TydiBinaryStream,
                                    post_titles: TydiBinaryStream,
                                    post_contents: TydiBinaryStream,
                                    post_author_username: TydiBinaryStream,
                                    post_tags: TydiBinaryStream,
                                    post_comments: TydiBinaryStream,
                                    post_comment_author_username: TydiBinaryStream,
                                    post_comment_content: TydiBinaryStream,
                                  ) {
    def reverse(): PhysicalStreamsTyped = {
      new PhysicalStreamsTyped(
        posts=TydiStream.fromBinaryBlobs(posts, 1),
        post_titles=TydiStream.fromBinaryBlobs(post_titles, 2),
        post_contents=TydiStream.fromBinaryBlobs(post_contents, 2),
        post_author_username=TydiStream.fromBinaryBlobs(post_author_username, 2),
        post_tags=TydiStream.fromBinaryBlobs(post_tags, 3),
        post_comments=TydiStream.fromBinaryBlobs(post_comments, 2),
        post_comment_author_username=TydiStream.fromBinaryBlobs(post_comment_author_username, 3),
        post_comment_content=TydiStream.fromBinaryBlobs(post_comment_content, 3),
      )
    }
  }

  object PhysicalStreamsBinary {
    def apply(posts: PhysicalStreamsTyped): PhysicalStreamsBinary = {
      new PhysicalStreamsBinary(
        posts=posts.posts.toBinaryBlobs(),
        post_titles=posts.post_titles.toBinaryBlobs(),
        post_contents=posts.post_contents.toBinaryBlobs(),
        post_author_username=posts.post_author_username.toBinaryBlobs(),
        post_tags=posts.post_tags.toBinaryBlobs(),
        post_comments=posts.post_comments.toBinaryBlobs(),
        post_comment_author_username=posts.post_comment_author_username.toBinaryBlobs(),
        post_comment_content=posts.post_comment_content.toBinaryBlobs(),
      )
    }
  }

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

  private val streamsTyped = PhysicalStreamsTyped(posts)
  private val streamsBinary = PhysicalStreamsBinary(streamsTyped)
  private val streamsTypedReconstructed = streamsBinary.reverse()

  println(s"Reconstructed first comment: ${streamsTypedReconstructed.post_comments.packets.head}")

  private val postsReconstructed = streamsTypedReconstructed.reverse()

  println(s"Reconstructed first post with comments: ${postsReconstructed.head}")
}
