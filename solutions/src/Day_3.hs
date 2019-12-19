module Day_3 where

import Protolude
import Data.List (intersect)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

type Input = (Text, Text)

data Point = Point
    { x :: Int
    , y :: Int
    }
    deriving (Eq, Show)

data Direction
    = R Int
    | L Int
    | U Int
    | D Int
    deriving (Eq, Show)

-- Way too slow, need to use lines with start and end points instead of [Point]
answer_1 :: Maybe Int
answer_1 =
    solve input

solve :: Input -> Maybe Int
solve input' =
    minimumMay (toList distances)
    where
        distances :: Seq Int
        distances =
            manhattanDistance origin <$> uncurry intersections (wirePaths input')

manhattanDistance :: Point -> Point -> Int
manhattanDistance Point{ x = x1, y = y1 } Point{ x = x2, y = y2 } =
    abs (x1 - x2) + abs (y1 - y2)

intersections :: Seq Point -> Seq Point -> Seq Point
intersections pts1 pts2 =
    Seq.fromList $ intersect (toList pts1) (toList pts2)

origin :: Point
origin =
    Point { x = 0, y = 0 }

wirePaths :: Input -> (Seq Point, Seq Point)
wirePaths (input1, input2) =
    (wirePath input1, wirePath input2)
    where
        wirePath :: Text -> Seq Point
        wirePath =
            path . mapMaybe toDirection . Text.splitOn ","

path :: [Direction] -> Seq Point
path dirs =
    path' dirs Seq.empty
    where
        path' dirs pts =
            case (dirs, pts) of
                ([], _) ->
                    pts
                (nextDir : dirs', _ :|> lastPoint) ->
                    path' dirs' (pts <> pathSegment lastPoint nextDir)
                (nextDir : dirs', _) ->
                    path' dirs' (pathSegment origin nextDir)

pathSegment :: Point -> Direction -> Seq Point
pathSegment pt@Point{ x, y } dir =
    case dir of
        R n ->
            addX pt <$> range n
        L n ->
            addX pt . negate <$> range n
        U n ->
            addY pt <$> range n
        D n ->
            addY pt . negate <$> range n
    where
        addX p n =
            p { x = x + n }
        addY p n =
            p { y = y + n }
        range n =
            Seq.fromList [1..n]

toDirection :: Text -> Maybe Direction
toDirection step =
    case Text.splitAt 1 step of
        ("R", rest) | Just n <- parseInt rest ->
            Just (R n)
        ("L", rest) | Just n <- parseInt rest ->
            Just (L n)
        ("U", rest) | Just n <- parseInt rest ->
            Just (U n)
        ("D", rest) | Just n <- parseInt rest ->
            Just (D n)
        _ ->
            Nothing
    where
        parseInt :: Text -> Maybe Int
        parseInt =
            readMaybe . Text.unpack

input :: Input
input =
    ( "R999,D666,L86,U464,R755,U652,R883,D287,L244,U308,L965,U629,R813,U985,R620,D153,L655,D110,R163,D81,L909,D108,L673,D165,L620,U901,R601,D561,L490,D21,R223,U478,R80,U379,R873,U61,L674,D732,R270,U297,L354,U264,L615,D2,R51,D582,R280,U173,R624,U644,R451,D97,R209,U245,R32,U185,R948,D947,R380,D945,L720,U305,R911,U614,L419,D751,L934,U371,R291,D166,L137,D958,R368,U441,R720,U822,R961,D32,R242,D972,L782,D166,L680,U111,R379,D155,R213,U573,R761,D543,R762,U953,R317,U841,L38,U900,R573,U766,R807,U950,R945,D705,R572,D994,L633,U33,L173,U482,R253,D835,R800,U201,L167,U97,R375,D813,L468,D924,L972,U570,R975,D898,L195,U757,L565,D378,R935,U4,L334,D707,R958,U742,R507,U892,R174,D565,L862,D311,L770,D619,L319,D698,L169,D652,L761,D644,R837,U43,L197,D11,L282,D345,L551,U460,R90,D388,R911,U602,L21,D275,L763,U880,R604,D838,R146,U993,L99,U99,R928,U54,L148,D863,R618,U449,R549,D659,R449,D435,L978,D612,L645,D691,R190,D434,L841,D364,L634,D590,R962,U15,R921,D442,L284,U874,R475,D556,L135,U376,L459,D673,L515,U438,L736,U266,L601,U351,R496,U891,L893,D597,L135,D966,R121,U763,R46,D110,R830,U644,L932,D122,L123,U145,R273,U690,L443,D372,R818,D259,L695,U69,R73,D718,R106,U929,L346,D291,L857,D341,R297,D823,R819,U496,L958,U394,R102,D763,L444,D835,L33,U45,R812,U845,R196,U458,R231,U637,R661,D983,L941,U975,L353,U609,L698,U152,R122,D882,R682,D926,R729,U429,R255,D227,R987,D547,L446,U217,R678,D464,R849,D472,L406,U940,L271,D779,R980,D751,L171,D420,L49,D271,R430,D530,R509,U479,R135,D770,R85,U815,R328,U234,R83"
    , "L1008,D951,L618,U727,L638,D21,R804,D19,L246,U356,L51,U8,L627,U229,R719,D198,L342,U240,L738,D393,L529,D22,R648,D716,L485,U972,L580,U884,R612,D211,L695,U731,R883,U470,R732,U723,R545,D944,R18,U554,L874,D112,R782,D418,R638,D296,L123,U426,L479,U746,L209,D328,L121,D496,L172,D228,L703,D389,R919,U976,R364,D468,L234,U318,R912,U236,R148,U21,R26,D116,L269,D913,L949,D206,L348,U496,R208,U706,R450,U472,R637,U884,L8,U82,L77,D737,L677,D358,L351,U719,R154,U339,L506,U76,L952,D791,L64,U879,R332,D244,R638,D453,L107,D908,L58,D188,R440,D147,R913,U298,L681,D582,L943,U503,L6,U459,L289,D131,L739,D443,R333,D138,R553,D73,L475,U930,L332,U518,R614,D553,L515,U602,R342,U95,R131,D98,R351,U921,L141,U207,R199,U765,R55,U623,R768,D620,L722,U31,L891,D862,R85,U271,R590,D184,R960,U149,L985,U82,R591,D384,R942,D670,R584,D637,L548,U844,R353,U496,L504,U3,L830,U239,R246,U279,L146,U965,R784,U448,R60,D903,R490,D831,L537,U109,R271,U306,L342,D99,L234,D936,R621,U870,R56,D29,R366,D562,R276,D134,L289,D425,R597,D102,R276,D600,R1,U322,L526,D744,L259,D111,R994,D581,L973,D871,R173,D924,R294,U478,R384,D242,R606,U629,R472,D651,R526,U55,R885,U637,R186,U299,R812,D95,R390,D689,R514,U483,R471,D591,L610,D955,L599,D674,R766,U834,L417,U625,R903,U376,R991,U175,R477,U524,L453,D407,R72,D217,L968,D892,L806,D589,R603,U938,L942,D940,R578,U820,L888,U232,L740,D348,R445,U269,L170,U979,L159,U433,L31,D818,L914,U600,L33,U159,R974,D983,L922,U807,R682,U525,L234,U624,L973,U123,L875,D64,L579,U885,L911,D578,R17,D293,L211"
    )
