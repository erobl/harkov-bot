module Constants (Wrd, Sentence, ChatId, startword, endword) where

type Wrd = String
type Sentence = [Wrd]
type ChatId = String

startword = "$secretstartword$"
endword = "$secretendword$"
