{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Life.DB.Books
   ( Author(..)
   , Book(..)
   , Books(..)
   )
where

import Data.Text
import Data.Set (Set)
import Data.Data (Data,Typeable)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.DateTime

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Life.DB.Tag


data Author = Author
   { authorName :: Text
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Author)


data Book = Book
   { bookTitle :: Text
   , bookAuthor :: Author
   , bookTags :: Set Tag
   , bookPages :: Int
   } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Book)

getBookTitle :: Query Book Text
getBookTitle = bookTitle <$> ask

getBookAuthor :: Query Book Author
getBookAuthor = bookAuthor <$> ask

getBookPages :: Query Book Int
getBookPages = bookPages <$> ask

$(makeAcidic ''Book ['getBookTitle, 'getBookAuthor, 'getBookPages])

data Books = Books
   { booksRead :: [(Book, Maybe DateTime)]
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Books)

getBooksRead :: Query Books [(Book,Maybe DateTime)]
getBooksRead = booksRead <$> ask

-- | Add a new read book
addReadBook :: Book -> Maybe DateTime -> Update Books ()
addReadBook book finishTime = do
   books <- get
   let rdBooks = booksRead books
   put (books { booksRead = (book,finishTime):rdBooks })

$(makeAcidic ''Books ['addReadBook, 'getBooksRead])
