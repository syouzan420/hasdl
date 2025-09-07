{-# LANGUAGE OverloadedStrings #-}
module MyAction (myAction,beforeDraw,updateCursor,afterDraw,makePList,changeAtr
                ,exeAttrCom,makeTextData,makeTexts,getCid,getPicIndex) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Linear.V2(V2(..))
import Linear.V4(V4(..))
import Data.Maybe(fromMaybe)
import Data.List(elemIndex)
import General (getIndex)
import MyLib (breakText,breakLine,nextPos)
import MyData (IsFormat,IsMarker,TextPos,Jump,FrJp,Mgn,Size
              ,TextData(..),ChPos(..),Cursor(..),ForText(..)
              ,State(..),Active(..),Attr(..),Rubi(..),Jumping(..),WMode(..)
              ,rubiSize,textLengthLimit,linkColor,selectColor,fontColor
              ,cursorColor,pinkColor,blueColor,cursorTime,imageNames)

type Index = Int
type FilePos = Int

myAction :: State -> State
myAction st = st

updateCursor :: Cursor -> Cursor
updateCursor (Cursor icr crc) =
  let ncrc = if crc<cursorTime then crc+1 else 0 
      nicr = if crc<cursorTime then icr else not icr
   in Cursor nicr ncrc

beforeDraw :: State -> State 
beforeDraw st = 
  let Cursor icrSt crcSt = cur$act st
      ncrc = if crcSt<cursorTime then crcSt+1 else 0
      nicr = if crcSt<cursorTime then icrSt else not icrSt
   in st{act=(act st){cur=Cursor nicr ncrc}}

afterDraw :: State -> State
afterDraw st = st

makeTextData :: State -> [TextData] 
makeTextData st =
  let ac = act st 
      texSt = tex ac
      etxSt = etx ac
      fortext = FT 0 (imk st) (ifm st) (wmd st) (szs st)
                                (fps ac) (tps ac) (wsz st) (mgn st) (atr st)
   in makeTexts fortext etxSt texSt

makeTexts :: ForText -> Text -> Text -> [TextData] 
makeTexts (FT ind imkSt ifmSt wmdSt szsSt fpsSt tpsSt wszSt mgnSt atrSt) etxSt texSt
 = let texSt' = if ifmSt then replaceText texSt else texSt 
   in case uncons texSt' of
    Nothing -> [TD True etxSt atrSt (makePList wmdSt wszSt mgnSt atrSt etxSt) 
                | texSt=="" && tpsSt==0] 
    Just (ch,tailTx) ->  
      let iMkr = tpsSt > ind 
          (natr,(ptx,pxs)) 
            | ifmSt = case ch of
               ';'-> exeAttrCom wmdSt szsSt fpsSt ind (changeAtr atrSt{ite=False} tailTx) 
               _  -> if cnm atrSt/=T.empty 
                      then exeAttrCom wmdSt szsSt fpsSt ind (atrSt{ite=False},texSt')
                      else (atrSt,T.break (==';') texSt')
            | imkSt && iMkr = (atrSt{fco=pinkColor},(texSt,T.empty))
            | otherwise = (atrSt{fco=fontColor},(texSt,T.empty))
          tll = textLengthLimit
          (ptx2,pxs2) = if T.length ptx>tll 
                            then (T.take tll ptx,T.drop tll ptx<>pxs)
                            else (ptx,pxs)
          lnTex = T.length texSt 
          preInc = lnTex - T.length pxs2 + 1
          iCur = tpsSt > ind && tpsSt < ind + preInc
          (iptx,tptx) = if iCur && not ifmSt && tpsSt>0 
                            then T.splitAt (tpsSt-ind) ptx2 else (ptx2,T.empty) 
          (tx,xs) = if iCur && not ifmSt 
                            then (iptx<>etxSt,tptx<>pxs2) else (ptx2,pxs2)
          (scrAt,fszAt) = (scr natr,fsz natr)
          fs = fromIntegral fszAt
          pList = makePList wmdSt wszSt mgnSt natr tx
          indInc = lnTex - T.length xs 
          CP _ _ lPos@(V2 lpx lpy) = last pList
          (V2 sx sy) = scrAt
          (V2 ww wh) = wszSt
          (V4 mr mt ml mb) = mgnSt
          nscr
            | iCur && wmdSt == T && lpx+sx < ml = V2 (ml-lpx) sy 
            | iCur && wmdSt == T && cnm natr/="rb" && lpx+sx > ww - mr - fs  
                                              = V2 (ww-mr-fs*2-lpx) sy
            | iCur && wmdSt == Y && lpy+sy > wh - mb - fs = V2 sx (wh-mb-fs-lpy)
            | iCur && wmdSt == Y && lpy+sy < mt = V2 sx (mt+fs-lpy)
            | otherwise = scrAt
      in TD iCur tx natr{gps=lPos,scr=nscr} pList:
              makeTexts (FT (ind+indInc) imkSt ifmSt wmdSt szsSt fpsSt
                             tpsSt wszSt mgnSt natr{gps=lPos,scr=nscr}) etxSt xs 

makePList :: WMode -> Size -> Mgn -> Attr -> Text -> [ChPos]
makePList wm ws mg at tx = 
  let (ps@(V2 ox oy),tw,nw) = (gps at,ltw at,lnw at)
   in case uncons tx of
    Nothing -> [CP False False ps]
    Just (ch,xs) -> let ((ihf,irt),(npos,_)) = nextPos ch tw nw wm ps ws mg (0,0) 
                        qtw = tw `div` 4
                        ihft = wm==T && ihf
                     in CP ihf irt (V2 (if ihft then ox+qtw else ox)
                                          (if ihft then oy-qtw else oy))
                        :makePList wm ws mg at{gps=npos} xs

replaceText ::Text -> Text
replaceText tx = T.replace "\n#" "\n;hi " $ 
                 T.replace "*" ";st " $
                 T.replace "\n>" "\n;qu " $
                 case uncons tx of 
  Nothing -> tx
  Just (ch,tailTx) -> case ch of
                        '>' -> ";qu " <> tailTx 
                        _ -> tx


changeAtr :: Attr -> Text -> (Attr, Text)
changeAtr attr tx = 
  let (ncid, (cm, rtx)) = getCid tx
      natr = attr{cnm=cm, cid=ncid}
   in (natr , rtx)

getCid :: Text -> (Int, (Text,Text))
getCid tx =
  let (cm,rtx) = T.break (==' ') tx
      ncid = case cm of
               "pic" -> 1
               "hi" -> 1
               "st" -> 1
               "qu" -> 1
               "rb" -> 2 
               "jtg" -> 1
               "jp" -> 3
               _    -> 0
   in (ncid, (cm, rtx))

getPicIndex :: Text -> Int
getPicIndex tx = let name = T.unpack tx 
                  in if name `elem` imageNames 
                           then getIndex name imageNames
                           else (-1) 

exeAttrCom :: WMode -> [Size] -> FilePos 
                      -> TextPos -> (Attr,Text) -> (Attr, (Text, Text))
exeAttrCom wmdSt szsSt fpsSt tpsSt (at,tx) = 
  let jmpAt = jmp at 
      (gpsAt,fszAt,ltwAt,rbiAt,dtaAt,jpsAt,fjpAt,sjnAt,cnmAt,cidAt) =
        (gps at,fsz at,ltw at,rbi at
        ,dta jmpAt,jps jmpAt,fjp jmpAt,sjn jmpAt,cnm at,cid at) 
      (Rubi rpsRb rwdRb tszRb tlwRb sprRb) = rbiAt
      tailTx = T.tail tx
      (ttx,rtx)
        | cidAt>0 = case cnmAt of
                      "qu" -> breakLine tailTx
                      "hi" -> breakLine tailTx
                      "st" -> T.break (==';') tailTx 
                      _    -> breakText tailTx
        | otherwise = T.break (==';') tailTx
      tln = T.length ttx
      tln' = fromIntegral tln
      ttx' = case cnmAt of
                "pic" -> case cidAt of
                          1 -> "pic " <> ttx 
                          _ -> ttx
                _     -> ttx
      rtx' = case cnmAt of
                "st" -> case cidAt of
                          1 -> T.drop 3 rtx 
                          _ -> rtx
                _    -> rtx
      natr = case cnmAt of
        "pic" -> case cidAt of 
                  1 -> let ind = getPicIndex ttx
                           (V2 iwd ihi) = if ind>=0
                              then szsSt!!ind else V2 0 0
                        in at{gps=if wmdSt==T then gpsAt-V2 iwd 0 
                                              else gpsAt+V2 0 ihi}
                  _ -> at
        "hi" -> case cidAt of
                  1 -> at{fsz=fszAt+3,fco=cursorColor}
                  0 -> at{fsz=fszAt-3,fco=fontColor}
                  _ -> at
        "st" -> case cidAt of
                  1 -> at{fco=selectColor}
                  0 -> at{fco=fontColor}
                  _ -> at
        "qu" -> case cidAt of
                  1 -> at{fco=pinkColor}
                  0 -> at{fco=fontColor}
                  _ -> at
        "rb" -> case cidAt of
                  2 -> at{rbi=rbiAt{rps=gpsAt,rwd=ltwAt*tln'}}
                  1 -> let fs = fromIntegral fszAt
                           rbStartPos = if wmdSt==T 
                              then rpsRb + V2 (fs+sprRb) 0  
                              else rpsRb - V2 0 (fromIntegral rubiSize+sprRb)
                           rbLetterWidth = rwdRb `div` tln' 
                        in at{gps=rbStartPos,fsz=rubiSize,ltw=rbLetterWidth 
                             ,rbi=rbiAt{tsz=fszAt,tlw=ltwAt}} 
                  0 -> at{gps=rpsRb+(if wmdSt==T then V2 0 rwdRb else V2 rwdRb 0)
                           ,fsz=tszRb, ltw=tlwRb}
                  _ -> at
        "jtg"-> case cidAt of
                  1 -> let jd = textToJumpData fpsSt tpsSt ttx
                           dataExist = jd `elem` jpsAt
                           njps = if dataExist then jpsAt else jpsAt ++ [jd]
                        in at{jmp=(jmp at){jps=njps},ite=True}
                  _ -> at
        "jp" -> case cidAt of
                  3 -> at{jmp=(jmp at){dta=[ttx]},ite=True} 
                  2 -> at{jmp=(jmp at){dta=dtaAt++[ttx]},ite=True} 
                  1 -> let tjp = searchJump jpsAt dtaAt tpsSt 
                           dataExist = tjp `elem` fjpAt
                           nfjp 
                             |fst tjp==(-1) = fjpAt
                             |dataExist = fjpAt
                             |otherwise = fjpAt ++ [tjp]
                           ind = fromMaybe (-1) $ elemIndex tjp nfjp
                           nfco
                             |sjnAt==ind = selectColor
                             |fst tjp/=(-1) = linkColor
                             |otherwise = fontColor
                        in at{jmp=(jmp at){fjp=nfjp},fco=nfco}
                  0 -> at{fco=fontColor} 
                  _ -> at
        _    -> at{cnm=""}
      ncnm = if cidAt==0 then "" else cnmAt
   in (natr{cnm=ncnm, cid=cidAt-1} , (if ite natr then "" else ttx', rtx'))

textToJumpData :: FilePos -> TextPos -> Text -> Jump
textToJumpData fpsSt tpsSt ttx = ((fpsSt,T.pack$show fpsSt),(tpsSt,ttx)) 

searchJump :: [Jump] -> [Text] -> TextPos -> FrJp 
searchJump [] _ _ = (-1,(0,0))
searchJump (((fi,fnm),(tp,tn)):xs) dtaAt tpsSt
  | length dtaAt/=2 = (-1,(0,0)) 
  | fnm==head dtaAt && tn==last dtaAt = (tpsSt,(fi,tp))
  | otherwise = searchJump xs dtaAt tpsSt

