module Text.Markdown.Smolder.Internal.Block 
  ( toElement
  , toElements
  , renderFencedCodeBlock
  ) where


import Prelude

import Control.Monad.Reader (ask)
import Data.Foldable (find, sequence_, traverse_)
import Data.List (List, range, length, zip)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Text.Markdown.SlamDown (Block(..), CodeBlockType(..), Inline, ListType(..))
import Text.Markdown.Smolder.Internal.Inline (encodeInlines, toInlineElements)
import Text.Markdown.Smolder.Internal.Type (ToReaderMarkup)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, attribute, text, (!))
import Text.Smolder.Markup as SM

toHeaderElement :: forall a e. Int -> List (Inline a) -> ToReaderMarkup e
toHeaderElement level is = do
  { options: { hideHeadingId, hClasses } } <- ask  
  inlineEls <- toInlineElements is
  let 
    el = HTML.parent ("h" <> show level) inlineEls
    hWithId = if hideHeadingId 
      then el 
      else el ! HA.id (encodeInlines is)
    cls = snd <$> find (((==) level) <<< fst) hClasses
  pure $ maybe hWithId (\c -> hWithId ! HA.className c) cls

-- | Convert Markdown block elements to Smolder markup
toElements :: forall a e. List (Block a) -> ToReaderMarkup e
toElements bs = sequence_ <$> traverse toElement bs

toListElement :: forall a e. Block a -> ToReaderMarkup e
toListElement block =
  case block of
    (Paragraph is) -> toInlineElements is
    _ -> toElement block

toListElements :: forall a e. ListType -> List (List (Block a)) -> ToReaderMarkup e
toListElements lstType bss = do 
  { options: { olClass, ulClass } } <- ask 
   
  case lstType of 
    (Bullet _) -> 
      let 
        ul' = if ulClass == "" 
          then HTML.ul 
          else HTML.ul ! HA.className ulClass
      in ul' <$> lstItems
    (Ordered _) -> 
      let 
        ol' = if olClass == "" 
          then HTML.ol 
          else HTML.ol ! HA.className olClass
      in ol' <$> lstItems
  where
    toBlockItems b = sequence_ <$> traverse toListElement b
    lstItems = sequence_ <$> traverse (\bs -> HTML.li <$> toBlockItems bs) bss
    
toCodeBlockContent :: forall e. List String -> Markup e
toCodeBlockContent ss = flip traverse_ (zip (range 1 (length ss)) ss) \(Tuple n s) -> 
  if n == 1 
    then text s 
    else do 
      HTML.br
      text s 


-- | Default render fenced code block. You can render yourself set it in options
renderFencedCodeBlock :: forall e. String -> List String -> Markup e  
renderFencedCodeBlock info ss = 
  HTML.pre 
    ! HA.className "code" 
    ! (attribute "data-lang" info) 
    $ HTML.code 
    $ toCodeBlockContent ss

toParagraphElement :: forall a e. List (Inline a) -> ToReaderMarkup e 
toParagraphElement is = do 
  { options: { pClass } } <- ask 
  let 
    p' = if pClass == ""
      then HTML.p
      else HTML.p ! HA.className pClass
  (pure <<< p') =<< toInlineElements is

-- | Convert Markdown block element to Smolder markup
toElement :: forall a e. Block a -> ToReaderMarkup e
toElement block = do 

  { options } <- ask
  case block of
    (Paragraph is) -> toParagraphElement is
    (Header n is) -> toHeaderElement n is
    (Blockquote bs) -> (pure <<< HTML.blockquote) =<< toElements bs
    (Lst lstType bss) -> toListElements lstType bss
    (CodeBlock Indented ss) -> 
      pure <<< HTML.pre <<< HTML.code $ toCodeBlockContent ss
    (CodeBlock (Fenced _ info) ss) -> pure $ options.renderFencedCodeBlock info ss

    (Rule) -> pure $ HTML.hr
    _ -> pure $ SM.empty
