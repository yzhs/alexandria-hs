module Alexandria.Types where

-- | A document type one of the strings "definition", "example", "lemma", "proposition" or "theorem"
type DocumentType = String

-- | Every document contains a list of tags.
type Tag = String

-- | A document id is a string encoded UUID.
type Id = String
