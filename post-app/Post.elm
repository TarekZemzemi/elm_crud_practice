module Post exposing (Post, PostId,idParser,idToString,postDecoder,emptyPost, postsDecoder,postEncoder ,newPostEncoder)
import Json.Decode as Decoder exposing (Decoder, list)
import Json.Decode exposing (list)
import Url.Parser exposing (Parser, custom)
import Json.Encode as Encode


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }
type PostId
    = PostId Int

postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


postDecoder : Decoder Post
postDecoder =
        Decoder.map4 Post
             (Decoder.field "id" idDecoder)
             (Decoder.field "title"  Decoder.string)
             (Decoder.field "authorName" Decoder.string)
             (Decoder.field "authorUrl" Decoder.string)

newPostEncoder : Post -> Encode.Value
newPostEncoder post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]

idDecoder : Decoder PostId
idDecoder =
    Decoder.map PostId Decoder.int

idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id

idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map PostId (String.toInt postId)

postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id

emptyPost : Post
emptyPost =
    { id = emptyPostId
    , title = ""
    , authorName = ""
    , authorUrl = ""
    }


emptyPostId : PostId
emptyPostId =
    PostId -1