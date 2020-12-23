module Day1a where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (List, fromFoldable, sort, reverse, (:))
import Data.Tuple (Tuple(..))

main :: Effect Unit
main = do
  log "The correct number is:"
  let Tuple a b = solve sortedNumbers sortedLargeNumbers
  log $ show (a * b)

solve :: List Int -> List Int -> Tuple Int Int
solve (x:xs) (y:ys) = case compare (x + y) 2020 of
    GT -> solve (x:xs) ys
    EQ -> Tuple x y
    LT -> solve xs (y:ys)
solve _ _ = Tuple 0 0

sortedNumbers :: List Int
sortedNumbers = sort numbers

sortedLargeNumbers :: List Int
sortedLargeNumbers = reverse sortedNumbers

numbers :: List Int
numbers =
    fromFoldable [ 1322
    , 1211
    , 1427
    , 1428
    , 1953
    , 1220
    , 1629
    , 1186
    , 1354
    , 1776
    , 1906
    , 1849
    , 1327
    , 1423
    , 401
    , 1806
    , 1239
    , 1934
    , 1256
    , 1223
    , 1504
    , 1365
    , 1653
    , 1706
    , 1465
    , 1810
    , 1089
    , 1447
    , 1983
    , 1505
    , 1763
    , 1590
    , 1843
    , 1534
    , 1886
    , 1842
    , 1878
    , 1785
    , 1121
    , 1857
    , 1496
    , 1696
    , 1863
    , 1944
    , 1692
    , 1255
    , 1572
    , 1767
    , 1509
    , 1845
    , 1479
    , 1935
    , 1507
    , 1852
    , 1193
    , 1797
    , 1573
    , 1317
    , 1266
    , 1707
    , 1819
    , 925
    , 1976
    , 1908
    , 1571
    , 1646
    , 1625
    , 1719
    , 1980
    , 1970
    , 1566
    , 1679
    , 1484
    , 1818
    , 1985
    , 1794
    , 1699
    , 1530
    , 1645
    , 370
    , 1658
    , 1345
    , 1730
    , 1340
    , 1281
    , 1722
    , 1623
    , 1148
    , 1545
    , 1728
    , 1325
    , 1164
    , 1462
    , 1893
    , 1736
    , 160
    , 1543
    , 1371
    , 1930
    , 1162
    , 2010
    , 1302
    , 1967
    , 1889
    , 1547
    , 1335
    , 1416
    , 1359
    , 1622
    , 1682
    , 1701
    , 1939
    , 1697
    , 1436
    , 1367
    , 1119
    , 1741
    , 1466
    , 1997
    , 1856
    , 1824
    , 1323
    , 1478
    , 1963
    , 1832
    , 1748
    , 1260
    , 1244
    , 1834
    , 1990
    , 1567
    , 1147
    , 1588
    , 1694
    , 1487
    , 1151
    , 1347
    , 1315
    , 1502
    , 546
    , 730
    , 1742
    , 1869
    , 1277
    , 1224
    , 1169
    , 1708
    , 1661
    , 174
    , 1207
    , 1801
    , 1880
    , 1390
    , 1747
    , 1215
    , 1684
    , 1498
    , 1965
    , 1933
    , 1693
    , 1129
    , 1578
    , 1189
    , 1251
    , 1727
    , 1440
    , 1178
    , 746
    , 1564
    , 944
    , 1822
    , 1225
    , 1523
    , 1575
    , 1185
    , 37
    , 1866
    , 1766
    , 1737
    , 1800
    , 1633
    , 1796
    , 1161
    , 1932
    , 1583
    , 1395
    , 1288
    , 1991
    , 229
    , 1875
    , 1540
    , 1876
    , 1191
    , 1858
    , 1713
    , 1725
    , 1955
    , 1250
    , 1987
    , 1724
    ]