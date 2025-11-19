# Scala Tydi Payload Kit

This library contains tools to transform Scala types (e.g. `case class` instances) into Tydi packages (original data + metadata) and binary blobs and back. This makes it easier to test and debug Tydi hardware.

Central to the library are **three representations of data**.

- A Scala data instance
  - This can be a sequence of case classes or base/ground data types such as `Int` or `Char`.
- A Tydi package
  - This contains the same data as the previous representation, but also metadata describing the nesting structure of the data according to the Tydi specification.
  - Specifically its fields are an `Option[T]` of the original datatype (can describe an empty sequence) and `last` bits to describe dimensionality information.
- A binary blob
  - Binary blobs are created from the package representation and can be sent to hardware.
  - Conversion to and from binary representation is handled by traits that have default implementations for ground types and are automatically derived for case classes by the [Magnolia library](https://github.com/softwaremill/magnolia) at compile time.

When using creating abstractions using this library, complex data transformations can be expressed and executed by simple functions. For example, the following code snippet constructs Tydi packages from a sequence of posts, converts them to binary blobs and then reconstructs the original data.
```scala
val posts: Seq[Post]          = ...
val streamsTyped              = PhysicalStreamsTyped(posts)
val streamsBinary             = PhysicalStreamsBinary(streamsTyped)
val streamsTypedReconstructed = streamsBinary.reverse()
val postsReconstructed        = streamsTypedReconstructed.reverse()

println(s"Reconstructed first post with comments: ${postsReconstructed.head}")
```

The lower level transforms look as follows. `TydiStream` objects can be created from any Scala sequence and then be "drilled down" in into nested fields. This creates the Tydi packages with correct metadata that can subsequently be converted to binary blobs.

```scala
val posts: Seq[Post]     = ...
val posts_tydi           = TydiStream.fromSeq(posts)

val titles_tydi          = posts_tydi.drill(_.title)
val titles_blobs         = titles_tydi.toBinaryBlobs

val tags_tydi            = posts_tydi.drill(_.tags).drill(x => x)
val tags_blobs           = posts_tydi.toBinaryBlobs

val comments_tydi        = posts_tydi.drill(_.comments)
val comment_author_tydi  = comments_tydi.drill(_.author.username)
val comment_author_blobs = comment_author_tydi.toBinaryBlobs
```

The reverse process of converting binary blobs back to Tydi packages looks as follows.

```scala
val post_tydi = TydiStream.fromBinaryBlobs(post_blobs, 1) // 1D stream

val titles_tydi          = TydiStream.fromBinaryBlobs(titles_blobs, 2) // 2D stream, one string of chars (1D) per post (1D) 

val tags_tydi            = TydiStream.fromBinaryBlobs(tags_blobs, 3) // 3D stream, one string of chars (1D) per tag in the list of tags (1D) per post (1D)

val comments_tydi        = TydiStream.fromBinaryBlobs(comments_blobs, 2)
val comment_author_tydi  = TydiStream.fromBinaryBlobs(comment_author_blobs, 3)

// For injection, we need to work depth-first, injecting the leaves into the parents.
// Here we first create the comments, inject strings, and then create the posts, injecting the comments and strings.
val comments_recreated = post_comments
  .injectString((c: Comment, s) => c.copy(author = c.author.copy(username = s)), post_comment_author_username)

val tags_recreated = post_tags.unpackToStrings()
val posts_recreated = posts
  .inject[Comment]((p: Post, s) => p.copy(comments = s.toList), comments_recreated)
  .inject[String]((p: Post, s) => p.copy(tags = s.toList), tags_recreated)
  .injectString((p: Post, s) => p.copy(title = s), post_titles)
```

Additional to the core functionality described above, the library contains a few utility methods to make managing (streams of) packages easier (e.g., creating bigger blobs that consist of multiple blobs concatenated together).
