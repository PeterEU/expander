{-# LANGUAGE BangPatterns #-}

module Base.Gui
  ( module Base.OHaskell
  , module Graphics.UI.Gtk
  , module Graphics.UI.Gtk.Gdk.PixbufAnimation
  , module System.Glib.GDateTime
  , Pos, Point, Path
  , addContextClass
  , cascade
  , fromInt, fromInt2
  , iconPixbuf
  , loadCSS
  , loadGlade
  , periodic
  , setBackground
  , spline
  , Text.unpack
  , Canvas(..), canvas, canvasSize, canvasBackground, canvasCursor
  , Color(..), black, white, red, green, blue, yellow, grey, magenta, cyan
  , orange, brown, darkGreen
  , GtkColor, gtkColor, toGtkColor, gtkGet, gtkSet, gtkDelay
  , None(..)
  , Anchor(..)
  , ArcStyle(..)
  , Arrow(..)
  , CapStyle(..)
  , JoinStyle(..)
  , AnchorType(..)
  , ArcStyleType(..)
  , ArrowType(..)
  , CapStyleType(..)
  , ImageFileType(..)
  , JoinStyleType(..)
  , ReliefType(..)
  , SelectType(..)
  , VertSide(..)
  , WrapType(..)
  , Align(..)
  , Background(..)
  , Fill(..)
  , Image(..)
  , Justify(..)
  , Outline(..)
  , Round(..)
  , Rotation(..)
  , Runnable(..)
  , Smooth(..)
  , Width(..)
  , ArcOpt(..), arcOpt
  , ImageOpt(..), imageOpt
  , LineOpt(..), lineOpt
  , MenuOpt(..), menuOpt
  , OvalOpt(..), ovalOpt
  , PolygonOpt(..), polygonOpt
  , TextOpt(..), textOpt
  ) where

import Prelude ()
import Base.OHaskell
import qualified Base.Haskell as Haskell
import qualified Codec.Picture as Picture
import Control.DeepSeq
import qualified Data.Text as Text
import Graphics.Rendering.Cairo as Cairo hiding (x,y,width,height,Path)
import Graphics.UI.Gtk hiding (Color,Font,ArrowType,Arrow,Fill,Table,ArrowClass,
                               Image,Star,Circle,Point,Dot,get,set)
import qualified Graphics.UI.Gtk as Gtk (get,set,Color(..))
import Graphics.UI.Gtk.Gdk.PixbufAnimation
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.Enums (PathPriorityType(PathPrioApplication))
import Graphics.UI.Gtk.General.StyleContext
import System.Directory
import System.FilePath
import System.Glib.GDateTime
            
type Pos   = (Int,Int)
type Point = (Double,Double)
type Path  = [Point]

data ImageFileType = PNG | PDF | PS | SVG deriving (Read,Show,Enum,Eq)

fromInt :: Int -> Double
fromInt = fromIntegral
       
fromInt2 :: Pos -> Point
fromInt2 (x,y) = (fromInt x,fromInt y)

-- main window icon file

stylePath :: IO FilePath
stylePath = do dataDir <- getDataDir
               return $ dataDir </> "style"

-- Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf for main window icon file

iconPixbuf :: IO Pixbuf
iconPixbuf = do sty <- stylePath
                pixbufNewFromFile $ sty </> ("icon" <.> "png")

pixbufRemoveAlpha :: Color -> Pixbuf -> IO ()
pixbufRemoveAlpha (RGB r g b) pixbuf = do
    hasAlpha <- pixbufGetHasAlpha pixbuf
    bitsPerSample <- pixbufGetBitsPerSample pixbuf
    nChannels <- pixbufGetNChannels pixbuf
    when (hasAlpha && bitsPerSample == 8 && nChannels == 4) $ do
         pixels <- pixbufGetPixels pixbuf :: IO (PixbufData Int Haskell.Word8)
         (start',end') <- Haskell.getBounds pixels
         let start = start' + nChannels - 1
             end = end' - 1
         mapM_ (setByteMax pixels) [start,start+nChannels .. end]
    done
    where setByteMax arr i = do 
                              alpha <- Haskell.readArray arr i
                              when (alpha == 0) $ do
                                   Haskell.writeArray arr (i-3) $ fromIntegral r
                                   Haskell.writeArray arr (i-2) $ fromIntegral g
                                   Haskell.writeArray arr (i-1) $ fromIntegral b
                                   Haskell.writeArray arr i maxBound

data Canvas = Canvas
    { canvasOval           :: Point -> Point -> OvalOpt -> IO ()
    , canvasArc            :: Point -> Double -> Point -> ArcOpt -> IO ()
    , canvasRectangle      :: Pos -> Pos -> OvalOpt -> IO ()
    , line                 :: [Pos] -> LineOpt -> IO ()
    , polygon              :: [Pos] -> PolygonOpt -> IO ()
    , canvasText           :: Pos -> TextOpt -> String -> IO ()
    , canvasImage          :: Pos -> ImageOpt -> Image -> IO ()
  --, cwindow              :: Pos -> [CWindowOpt] -> IO CWindow
    , clear                :: IO ()
    , canvasSave           :: FilePath -> IO FilePath
    , canvasSetSize        :: Pos -> IO ()
    , canvasGetSize        :: IO Pos
    , canvasSetBackground  :: Color -> IO ()
    , canvasGetBackground  :: IO Color
    , getDrawingArea       :: DrawingArea
    , getSurface           :: IO Surface
    , getTextWidth         :: FontDescription -> String -> IO Double
    , getTextHeight        :: FontDescription -> IO Pos
    , canvasSetCursor      :: CursorType -> IO () }

-- Canvas GLib attributes

canvasSize :: Attr Canvas Pos
canvasSize = newNamedAttr "size" canvasGetSize canvasSetSize

canvasBackground :: Attr Canvas Color
canvasBackground = newNamedAttr "background" canvasGetBackground 
                                             canvasSetBackground

canvasCursor :: WriteAttr Canvas CursorType
canvasCursor = writeNamedAttr "cursor" canvasSetCursor

-- SMOOTHING

smooth,spline :: Path -> Path

smooth ps = spline $!! spline ps 

-- used by line,polygon

-- spline ps builds a B-spline with degree 3 with control points ps. 

spline ps@(p:_:_:_) = 
    if p == last ps then spl True $!! init ps else spl False ps
    where cadd (x1,y1) (x2,y2) = (r1,r2) where !r1 = x1+x2; !r2 = y1+y2
          spl closed ps = first:map f [1..resolution] ++ map g [1..9] ++
                          [if closed then first else last ps] where
              first = f 0; !n = length ps; !resolution = n*6
              upb = m - if closed then 1 else 3 where !m = fromInt n
              f i = point $!! upb*fromInt i/fromInt resolution 
              g i = point $!! upb+fromInt i/10
              h d s = if d == 0 then 0 else s
              t i = if i < 3 then 0 else fromInt (min i n)-2
              u i = if i <= n then fromInt i else u (i-1)+u (i-n)-u (i-n-1)
              point v = Haskell.foldl1' cadd $!! map h [0..n-1] where
                        h i = (r1,r2) where
                              (x,y) = ps!!i; !r1 = x*z; !r2 = y*z
                              z | closed && v < u i = blend1 u i $!! v-u 0+u n
                                | closed            = blend1 u i v
                                | True              = blend1 t i v 
              blend1,blend2 :: (Int -> Double) -> Int -> Double -> Double
              blend1 t i v = h denom1 sum1 + h denom2 sum2
                             where a = t i; b = t $!! i+3
                                   num1 = v-a; num2 = b-v
                                   denom1 = t (i+2)-a; denom2 = b-t (i+1)
                                   sum1 = num1/denom1*blend2 t i v
                                   sum2 = num2/denom2*blend2 t (i+1) v
              blend2 t i v = h denom1 sum1 + h denom2 sum2
                             where a = t i; b = t $ i+1; c = t $ i+2
                                   num1 = v-a; num2 = c-v
                                   denom1 = b-a; denom2 = c-b
                                   p i = t i <= v && v < t (i+1)
                                   sum1 = if p i then num1/denom1 else 0
                                   sum2 = if p $ i+1 then num2/denom2 else 0
spline ps = ps

-- used by smooth and Epaint > hulls,splinePict
                                
calcVertexes :: Pos -> Pos -> (Point,Point)
calcVertexes tip end = ((x1,y1), (x2,y2))
                       where len = 7; degree = 0.5
                             (tipX,tipY) = fromInt2 tip
                             (endX,endY) = fromInt2 end
                             angle  = atan2 (tipY-endY) (tipX-endX) + pi
                             x1 = tipX + len * cos (angle - degree)
                             y1 = tipY + len * sin (angle - degree)
                             x2 = tipX + len * cos (angle + degree)
                             y2 = tipY + len * sin (angle + degree)
        
arrow :: Maybe ArrowType -> [Pos] -> Render ()
arrow (Just First) (p1:p2:_) = arrowTriangle p1 p2
arrow (Just Last) ps@(_:_:_) = arrowTriangle p1 p2 
                    where (p2,p1) = last2 ps
                          last2 (p1:[p2]) = (p1,p2)
                          last2 (_:ps)    = last2 ps
                          last2 []       = error "Gui.arrow.last2 applied to []"
arrow (Just Both) ps@(_:_:_) = arrow (Just First) ps >> arrow (Just Last) ps
arrow _ _ = return ()
                   
arrowTriangle :: Pos -> Pos -> Render ()
arrowTriangle tip@(tipX, tipY) end = do moveTo (fromInt tipX) $ fromInt tipY
                                        lineTo x2 y2; lineTo x3 y3
                                        closePath
                                        Cairo.fill 
                                  where ((x2,y2),(x3,y3)) = calcVertexes tip end

curve :: Path -> Render ()
curve ((x1,y1):(x2,y2):(x3,y3):ps) = do curveTo x1 y1 x2 y2 x3 y3; curve ps
curve ((x,y):_)                    = lineTo x y
curve _                            = return () 

setColor :: Color -> Render ()
setColor (RGB r g b) = setSourceRGB (fromInt r / 255) (fromInt g / 255)
                                    (fromInt b / 255)
        
getAnchorPos :: Double -> Double -> Double -> Double -> AnchorType -> Point
getAnchorPos x y width height anchor = (x',y') 
        where x' = if anchor `elem` [NW,W,SW] then x
                   else if anchor `elem` [NE,E,SE] then x-width else x-width/2
              y' = if anchor `elem` [NW,N,NE] then y
                   else if anchor `elem` [SW,S,SE] then y-height else y-height/2
              
setLineOpt :: LineOpt -> Render ()
setLineOpt (LineOpt col wdth _ cap joinStyle aa _) = do
                   setColor col
                   setLineWidth wdth
                   setLineCap cap
                   setLineJoin joinStyle
                   setAntialias $ if aa then AntialiasDefault else AntialiasNone

canvas :: IO Canvas
canvas = do
    surfaceRef <- newIORef undefined
    sizeRef <- newIORef (0, 0)
    backgroundRef <- newIORef $ RGB 0 0 0
    surface0 <- createImageSurface FormatRGB24 1 1
    writeIORef surfaceRef surface0
    drawingArea <- drawingAreaNew
    on drawingArea draw $ do surface <- liftIO $ readIORef surfaceRef
                             setSourceSurface surface 0 0
                             paint
    return $ let
        
        canvasOval :: Point -> Point -> OvalOpt -> IO ()
        canvasOval (x,y) (rx,ry) opt = do
            surface <- readIORef surfaceRef
            renderWith surface $ do save
                                    translate x y
                                    scale rx ry
                                    arc 0 0 1 0 (2 * pi)
                                    restore
                                    finishOval opt
            widgetQueueDraw drawingArea

        canvasArc :: Point -> Double -> Point -> ArcOpt -> IO ()
        canvasArc (x,y) radius (angle1,angle2) 
                        opt@ArcOpt {arcStyle = style,
                                    arcOutline = col,
                                    arcFill = c} = do
            surface <- readIORef surfaceRef
            renderWith surface $ do
                save
                setLineWidth $ arcWidth opt
                -- draw arc
                arc x y radius angle1 angle2
                -- draw chord
                when (style == Chord) closePath
                -- draw slice
                when (style == Pie) $ do lineTo x y
                                         closePath
                when (Haskell.isJust c && (style /= Perimeter)) $ 
                                      do setColor $ Haskell.fromJust c
                                         fillPreserve
                setColor col
                stroke
                restore
            widgetQueueDraw drawingArea
            
        canvasImage :: Pos -> ImageOpt -> Image -> IO ()
        canvasImage pos opt (Image alpha image) = do
          when (not alpha) $ do rgb <- readIORef backgroundRef
                                pixbufRemoveAlpha rgb image
          surface <- readIORef surfaceRef
          width <- pixbufGetWidth image
          height <- pixbufGetHeight image
          let (x,y)   = fromInt2 pos
              (x',y') = getAnchorPos x y (fromInt width)(fromInt height)
                                         $ imageAnchor opt
          renderWith surface $ do setSourcePixbuf image x' y'
                                  rotate $ imageRotate opt
                                  scale sclFactor sclFactor
                                  paint
          widgetQueueDraw drawingArea
          where sclFactor   = imageScale opt
        
        finishOval :: OvalOpt -> Render ()
        finishOval opt@OvalOpt {ovalFill = c} = do
            save
            when (Haskell.isJust c) $ do setColor $ Haskell.fromJust c
                                         fillPreserve
            setColor $ ovalOutline opt
            setLineWidth $ ovalWidht opt
            stroke
            restore
        
        line :: [Pos] -> LineOpt -> IO ()
        line [] _ = done
        line ps opt@LineOpt {lineArrow = at, lineSmooth = isSmooth} = do
            surface <- readIORef surfaceRef
            let dps = map fromInt2 ps
                ((x,y):qs) = if isSmooth then smooth dps else dps
            renderWith surface $ do 
                        save
                        setLineOpt opt
                        moveTo x y
                        if isSmooth then curve qs else mapM_ (uncurry lineTo) qs
                        stroke
                        arrow at ps
                        restore
            widgetQueueDraw drawingArea
        
        polygon :: [Pos] -> PolygonOpt -> IO ()
        polygon [] _ = error "Gui.polygon: empty list."
        polygon ps opt@PolygonOpt {polygonFill = c, 
                                   polygonAntialias = aa,
                                   polygonSmooth = isSmooth} = do
            surface <- readIORef surfaceRef
            let dps = map fromInt2 ps
                ((x,y):qs) = if isSmooth then smooth dps else dps
            renderWith surface $ do
                   save
                   setAntialias $ if aa then AntialiasDefault else AntialiasNone
                   setLineWidth $ polygonWidth opt
                   moveTo x y
                   if isSmooth then curve qs else mapM_ (uncurry lineTo) qs
                   closePath
                   when (Haskell.isJust c) $ do setColor $ Haskell.fromJust c
                                                fillPreserve
                   setColor $ polygonOutline opt
                   stroke
                   restore
            widgetQueueDraw drawingArea
        
        canvasRectangle :: Pos -> Pos -> OvalOpt -> IO ()
        canvasRectangle pos dim opt = do
            surface <- readIORef surfaceRef
            renderWith surface $ do save
                                    Cairo.rectangle x y width height
                                    restore
                                    finishOval opt
            widgetQueueDraw drawingArea
            where (x,y) = fromInt2 pos
                  (width,height) = fromInt2 dim

        canvasSave file = do
          createDirectoryIfMissing True $ takeDirectory file
          let ext = takeExtension file
              format = dups $ map toLower $ if null ext then ext else tail ext
              formattext = Text.pack format
          surface <- readIORef surfaceRef
          width <- imageSurfaceGetWidth surface
          height <- imageSurfaceGetHeight surface
          -- workaround for gif
          if format == "gif" then do let tmpfile = file -<.> "png"
                                     canvasSave tmpfile
                                     eitherimg <- Picture.readImage tmpfile
                                     either (const $ putStrLn "gif failed") id $ 
                                                do img <- eitherimg
                                                   Picture.saveGifImage file img
                                     exist <- doesFileExist tmpfile
                                     when exist $ removeFile tmpfile        
          -- vector
          else if format `elem` words "ps pdf svg"
            then do let withSurface = case format of
                            "pdf" -> withPDFSurface
                            "ps"  -> withPSSurface
                            "svg" -> withSVGSurface
                            _ -> error $ "Gui.Canvas.canvasSave: Vector format "
                                         ++ format ++ " should never happen."
                    withSurface file (fromInt width) (fromInt height)
                                $ \image -> renderWith image $ do
                                                    setSourceSurface surface 0 0
                                                    paint
          -- pixel
          else if formattext `elem` pixbufGetFormats
               then do pbuf <- pixbufNewFromSurface surface 0 0 width height
                       pixbufSave pbuf file formattext ([] :: [(String,String)])
               else putStrLn "format not supported"
          return file
          where dups "jpg" = "jpeg"
                dups "eps" = "ps"
                dups name  = name
        
        canvasSetCursor :: CursorType -> IO ()
        canvasSetCursor ct = do
            cursor <- cursorNew ct
            root <- widgetGetRootWindow drawingArea
            drawWindowSetCursor root $ Just cursor
        
        canvasSetSize size@(width, height) = do
            writeIORef sizeRef size
            background <- readIORef backgroundRef
            surface <- createImageSurface FormatRGB24 width height
            gtkSet drawingArea [widgetWidthRequest  := width,
                                widgetHeightRequest := height]
            renderWith surface $ do setColor background
                                    paint
            writeIORef surfaceRef surface
        
        canvasText :: Pos -> TextOpt -> String -> IO ()
        canvasText (x,y) (TextOpt fnt align anchor col) str = do
            surface <- readIORef surfaceRef
            renderWith surface $ do
                save
                setColor col
                layout <- createLayout str
                liftIO $ layoutSetFontDescription layout fnt
                liftIO $ layoutSetAlignment layout 
                       $ case align of LeftAlign   -> AlignLeft
                                       CenterAlign -> AlignCenter
                                       RightAlign  -> AlignRight
                (PangoRectangle xBearing yBearing w ht,_) 
                                             <- liftIO $ layoutGetExtents layout
                let baseX = fromInt x - xBearing
                    baseY = fromInt y -- - yBearing
                    (x',y') = getAnchorPos baseX baseY w ht anchor
                updateLayout layout
                moveTo x' $ baseY-8
                showLayout layout
                restore
            widgetQueueDraw drawingArea
               
        clear = do
            surface <- readIORef surfaceRef
            background <- readIORef backgroundRef
            renderWith surface $ do setColor background
                                    paint
                                    
        getTextHeight font = do
            surface <- readIORef surfaceRef
            textHeight <- newIORef undefined
            renderWith surface $ do
                 layout <- createLayout "BASE"
                 liftIO $ layoutSetFontDescription layout $ Just font
                 (PangoRectangle _ _ _ ht,_) <- liftIO $ layoutGetExtents layout
                 liftIO $ writeIORef textHeight (floor $ ht/2,floor $ (ht+ht)/3)
            readIORef textHeight
        
        getTextWidth font str = do
            surface <- readIORef surfaceRef
            textWidth <- newIORef undefined
            renderWith surface $ do
                 layout <- createLayout str
                 liftIO $ layoutSetFontDescription layout $ Just font
                 (PangoRectangle _ _ w _,_) <- liftIO $ layoutGetExtents layout
                 liftIO $ writeIORef textWidth w
            readIORef textWidth
        
      in Canvas {canvasOval          = canvasOval,
                 canvasArc           = canvasArc,
                 line                = line,
                 polygon             = polygon,
                 canvasRectangle     = canvasRectangle,
                 canvasText          = canvasText,
                 canvasImage         = canvasImage,
                 clear               = clear,
                 canvasSave          = canvasSave,
                 canvasSetSize       = canvasSetSize,
                 canvasGetSize       = readIORef sizeRef,
                 canvasSetBackground = writeIORef backgroundRef,
                 canvasGetBackground = readIORef backgroundRef,
                 getDrawingArea      = drawingArea,
                 getSurface          = readIORef surfaceRef,
                 getTextWidth        = getTextWidth,
                 getTextHeight       = getTextHeight,
                 canvasSetCursor     = canvasSetCursor}

loadCSS :: IO ()
loadCSS = do
    provider <- cssProviderNew
    Just display <- displayGetDefault
    screen <- displayGetDefaultScreen display
    styleContextAddProviderForScreen screen provider $
                                     fromEnum PathPrioApplication
    path <- stylePath
    cssProviderLoadFromPath provider $ path </> ("style" <.> "css")

loadGlade :: String -> IO Builder
loadGlade glade = do
    builder <- builderNew
    path <- stylePath
    builderAddFromFile builder $ path </> (glade <.> "glade")
    return builder

setBackground :: WidgetClass widget => widget -> Background -> IO ()
setBackground w (Background name) = do
    sc <- widgetGetStyleContext w
    classes <- styleContextListClasses sc :: IO [String]
    let f cl = when (Haskell.isPrefixOf "bg_" cl) $ 
                    styleContextRemoveClass sc cl
    Haskell.forM_ classes f
    styleContextAddClass sc name

addContextClass :: WidgetClass widget => widget -> String -> IO ()
addContextClass widget cl = do
    context <- widgetGetStyleContext widget
    styleContextAddClass context cl

data Color = RGB Int Int Int deriving (Read,Eq)

black,white,red,green,blue,yellow,grey,magenta,cyan,orange,brown,darkGreen 
         :: Color
black     = RGB 0 0 0
white     = RGB 255 255 255
red       = RGB 255 0 0
green     = RGB 0 255 0
blue      = RGB 0 0 255
yellow    = RGB 255 255 0
grey      = RGB 200 200 200
magenta   = RGB 255 0 255
cyan      = RGB 0 255 255
orange    = RGB 255 180 0
brown     = RGB 0 160 255
darkGreen = RGB 0 150 0

type GtkColor = Gtk.Color

gtkColor :: Haskell.Word16 -> Haskell.Word16 -> Haskell.Word16 -> GtkColor
gtkColor = Gtk.Color

toGtkColor :: Color -> GtkColor
toGtkColor (RGB r g b) = Gtk.Color (fromIntegral r) (fromIntegral g) 
                                   $ fromIntegral b

gtkGet :: o -> ReadWriteAttr o a b -> IO a
gtkGet = Gtk.get

gtkSet :: o -> [AttrOp o] -> IO ()
gtkSet = Gtk.set

-- option types

data None          = None
data Align         = LeftAlign | CenterAlign | RightAlign
data Anchor        = Anchor AnchorType
data AnchorType    = NW | N | NE | W | C | E | SW | S | SE deriving Eq
data ArcStyle      = ArcStyle ArcStyleType
data ArcStyleType  = Pie | Chord | Perimeter deriving (Read,Eq)
data Arrow a       = Arrow a
data ArrowType     = First | Last | Both deriving (Show, Eq, Enum)
data Background    = Background String
data CapStyle a    = CapStyle a
data CapStyleType  = Butt | Proj 
data Fill          = Fill Color
data Image         = Image Bool Pixbuf
data JoinStyle a   = JoinStyle a
data JoinStyleType = Bevel | Miter 
data Justify       = Justify Align
data Outline       = Outline Color
data Rotation      = Counterclockwise | RotateUpsidedown | RotateClockwise
data Smooth        = Smooth Bool
data Width         = Width Double
data ReliefType    = Raised | Sunken | Flat | Ridge | Solid | Groove
data Round         = Round
data SelectType    = Single | Multiple
data VertSide      = Top | Bottom 
data WrapType      = NoWrap | CharWrap | WordWrap deriving (Show, Eq, Enum)

data TextOpt = TextOpt {textFont :: Maybe FontDescription,
                        textJustify :: Align,
                        textAnchor :: AnchorType,
                        textFill :: Color}

textOpt :: TextOpt
textOpt = TextOpt Nothing LeftAlign C black

data LineOpt = LineOpt {lineFill :: Color,
                        lineWidth :: Double,
                        lineArrow :: Maybe ArrowType,
                        lineCapStyle :: LineCap,
                        lineJoinStyle :: LineJoin,
                        lineAntialias :: Bool,
                        lineSmooth    :: Bool} deriving (Show,Eq)

lineOpt :: LineOpt
lineOpt = LineOpt black 1 Nothing LineCapButt LineJoinMiter True False

data PolygonOpt = PolygonOpt {polygonAntialias :: Bool,
                              polygonOutline   :: Color,
                              polygonFill      :: Maybe Color,
                              polygonWidth     :: Double,
                              polygonSmooth    :: Bool}
                              
polygonOpt :: PolygonOpt
polygonOpt = PolygonOpt True black Nothing 1 False

data ArcOpt = ArcOpt {arcFill    :: Maybe Color,
                      arcWidth   :: Double,
                      arcOutline :: Color,
                      arcStyle   :: ArcStyleType}

arcOpt :: ArcOpt
arcOpt = ArcOpt Nothing 1 black Pie

data OvalOpt = OvalOpt {ovalFill    :: Maybe Color,
                        ovalWidht   :: Double,
                        ovalOutline :: Color}

ovalOpt :: OvalOpt
ovalOpt = OvalOpt Nothing 1 black

data ImageOpt = ImageOpt {imageRotate :: Double,
                          imageScale  :: Double,
                          imageAnchor :: AnchorType}

imageOpt :: ImageOpt
imageOpt = ImageOpt 0.0 1.0 C

imageGetSize :: Image -> IO Pos
imageGetSize (Image _ buf) = do x <- pixbufGetWidth buf 
                                y <- pixbufGetHeight buf
                                return (x,y)

-- data MenuOpt    > WindowOpt, Enabled
-- data MButtonOpt > StdOpt, FontOpt, PadOpt, Img, Btmp, Underline, 

instance Show ArcStyleType where show Pie       = "pieslice"
                                 show Chord     = "chord"
                                 show Perimeter = "arc"

instance Show Color where 
      showsPrec _ (RGB r g b) rest = "#" ++ concatMap (hex 2 "") [r,g,b] ++ rest
                        where hex :: Int -> [Char] -> Int -> [Char]
                              hex 0 rs _ = rs
                              hex t rs 0 = hex (t-1) ('0':rs) 0
                              hex t rs i = hex (t-1)(chr (48+m+7*div m 10):rs) d
                                       where m = mod i 16; d = div i 16

-- TODO What is the String for?

gtkDelay :: Int -> IO () -> IO ()
gtkDelay millisecs act = Haskell.void $ timeoutAdd (act>>return False) millisecs

data Runnable = Runnable {runnableStart :: IO (), runnableStop :: IO ()}
    
-- periodic uses Graphics.UI.Gtk.General.General.timoutAdd from thegtk3 package 
-- to synchronize with GTK3 main loop
-- Tcl/Tk version does something similar

periodic :: Int -> IO () -> IO Runnable
periodic millisecs act = do
    handlerID <- newIORef Nothing -- Nothing == not running
    return Runnable {runnableStart = do
                         maybeID <- readIORef handlerID
                         when (Haskell.isNothing maybeID) $ do    -- not running
                              hID <- timeoutAdd (act >> return True) millisecs
                              writeIORef handlerID $ Just hID,
                     runnableStop  = do
                         maybeID <- readIORef handlerID
                         when (Haskell.isJust maybeID) $ do       -- running
                              timeoutRemove $ Haskell.fromJust maybeID
                              writeIORef handlerID Nothing}

data MenuOpt = MenuOpt {menuFont :: Maybe String,
                        menuBackground :: Maybe Background}
                        
menuOpt :: MenuOpt
menuOpt = MenuOpt {menuFont = Nothing, menuBackground = Nothing}

-- Tk.Menu.cascade

cascade :: Menu -> String -> MenuOpt -> IO Menu
cascade menu label MenuOpt {menuFont = mFont, menuBackground = bg} = do
                 item <- menuItemNewWithLabel label
                 menuShellAppend menu item
                 doMaybe (addContextClass item) mFont
                 doMaybe (setBackground item) bg
                 subMenu <- menuNew
                 gtkSet item [menuItemSubmenu := subMenu, widgetVisible := True]
                 return subMenu
                 where doMaybe act (Just v) = act v

                       doMaybe _ Nothing    = done

