{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad              ((<=<))
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.Text                  as T
import           Network.Mail.Mime
import           System.Random              (StdGen, mkStdGen)

stdgen :: StdGen
stdgen = mkStdGen 1

testMail :: Alternatives -> [(T.Text,T.Text,FilePath)] -> [(T.Text,FilePath)] -> IO Mail
testMail alts inline atts =
  (addAttachments atts <=< addInlineAttachments inline) m
  where
    m = addPart alts
     $ emptyMail (Address (Just "Ben Kolera") "ben.kolera@gmail.com")

testPlainPart :: Part
testPlainPart = plainPart "This is a plain text part"

testHtmlPart :: Part
testHtmlPart = htmlPart "<div>This is a html text part</div>"

printMail :: IO Mail -> IO ()
printMail mkMail = do
  mail <- mkMail
  LBSC8.putStrLn . fst $ renderMail stdgen mail

inlineAttachment :: (T.Text,T.Text,FilePath)
inlineAttachment = ("image/gif","nyancat.gif","nyancat.gif")

attachment :: (T.Text,FilePath)
attachment = ("application/pdf","stuff.pdf")

-- |
-- >>> printMail (testMail [testPlainPart] [] [])
-- From: =?utf-8?Q?Ben_Kolera?= <ben.kolera@gmail.com>
-- MIME-Version: 1.0
-- Content-Type: text/plain; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- This is a plain text part
-- >>> printMail (testMail [testPlainPart,testHtmlPart] [] [])
-- From: =?utf-8?Q?Ben_Kolera?= <ben.kolera@gmail.com>
-- MIME-Version: 1.0
-- Content-Type: multipart/alternative; boundary="bGXfgLY1EQ"
-- <BLANKLINE>
-- --bGXfgLY1EQ
-- Content-Type: text/plain; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- This is a plain text part
-- --bGXfgLY1EQ
-- Content-Type: text/html; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- <div>This is a html text part</div>
-- --bGXfgLY1EQ--
-- >>> printMail (testMail [testPlainPart,testHtmlPart] [inlineAttachment] [])
-- From: =?utf-8?Q?Ben_Kolera?= <ben.kolera@gmail.com>
-- MIME-Version: 1.0
-- Content-Type: multipart/related; boundary="7RqZgvAiW0"
-- <BLANKLINE>
-- --7RqZgvAiW0
-- Content-Type: image/gif
-- Content-Transfer-Encoding: base64
-- Content-ID: <nyancat.gif>
-- <BLANKLINE>
-- VGhpcyBpcyBub24gcmVhbGx5IGFuIGltYWdlLg==
-- <BLANKLINE>
-- --7RqZgvAiW0
-- Content-Type: multipart/alternative; boundary="bGXfgLY1EQ"
-- <BLANKLINE>
-- --bGXfgLY1EQ
-- Content-Type: text/plain; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- This is a plain text part
-- --bGXfgLY1EQ
-- Content-Type: text/html; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- <div>This is a html text part</div>
-- --bGXfgLY1EQ--
-- --7RqZgvAiW0--
-- >>> printMail (testMail [testPlainPart,testHtmlPart] [inlineAttachment] [attachment])
-- From: =?utf-8?Q?Ben_Kolera?= <ben.kolera@gmail.com>
-- MIME-Version: 1.0
-- Content-Type: multipart/mixed; boundary="SlIk5BhKRw"
-- <BLANKLINE>
-- --SlIk5BhKRw
-- Content-Type: multipart/related; boundary="7RqZgvAiW0"
-- <BLANKLINE>
-- --7RqZgvAiW0
-- Content-Type: image/gif
-- Content-Transfer-Encoding: base64
-- Content-ID: <nyancat.gif>
-- <BLANKLINE>
-- VGhpcyBpcyBub24gcmVhbGx5IGFuIGltYWdlLg==
-- <BLANKLINE>
-- --7RqZgvAiW0
-- Content-Type: multipart/alternative; boundary="bGXfgLY1EQ"
-- <BLANKLINE>
-- --bGXfgLY1EQ
-- Content-Type: text/plain; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- This is a plain text part
-- --bGXfgLY1EQ
-- Content-Type: text/html; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- <div>This is a html text part</div>
-- --bGXfgLY1EQ--
-- --7RqZgvAiW0--
-- --SlIk5BhKRw
-- Content-Type: application/pdf
-- Content-Transfer-Encoding: base64
-- Content-Disposition: attachment; filename=stuff.pdf
-- <BLANKLINE>
-- VGhpcyBpcyBub3QgcmVhbGx5IGEgcGRmLg==
-- <BLANKLINE>
-- --SlIk5BhKRw--
-- >>> printMail (testMail [testPlainPart,testHtmlPart] [] [attachment])
-- From: =?utf-8?Q?Ben_Kolera?= <ben.kolera@gmail.com>
-- MIME-Version: 1.0
-- Content-Type: multipart/mixed; boundary="7RqZgvAiW0"
-- <BLANKLINE>
-- --7RqZgvAiW0
-- Content-Type: multipart/alternative; boundary="bGXfgLY1EQ"
-- <BLANKLINE>
-- --bGXfgLY1EQ
-- Content-Type: text/plain; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- This is a plain text part
-- --bGXfgLY1EQ
-- Content-Type: text/html; charset=utf-8
-- Content-Transfer-Encoding: quoted-printable
-- <BLANKLINE>
-- <div>This is a html text part</div>
-- --bGXfgLY1EQ--
-- --7RqZgvAiW0
-- Content-Type: application/pdf
-- Content-Transfer-Encoding: base64
-- Content-Disposition: attachment; filename=stuff.pdf
-- <BLANKLINE>
-- VGhpcyBpcyBub3QgcmVhbGx5IGEgcGRmLg==
-- <BLANKLINE>
-- --7RqZgvAiW0--
main :: IO ()
main = return ()
