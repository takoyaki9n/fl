import Control.Monad.Error

data ParseError = Err {location::Int, reason::String}

instance Error ParseError where
  noMsg = Err 0 "ParseError"
  strMsg s = Err 0 s

type ParseMonad = Either ParseError

parseHexDigit c idx = if isHexDigit c then
                        return $ toInteger $ digitToInt c
                      else
                        throwError (Err idx ([c]))

isHexDigit c = ('a' <= c && c <= 'f') || ('0' <= c && c <= '9')
digitToInt c