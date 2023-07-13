(#s(row "0000" (success immediate) ())
 #s(row "0001" (success immediate) ())
 #s(row "0002" (success immediate) ())
 #s(row "0010" (success (2 0)) ("0220" "0121" "1111"))
 #s(row "0020" (success (1 0)) ("0110" "0212" "1212" "1112"))
 #s(row "0011" (success (1 3)) ("0111"))
 #s(row "0012" (success (1 2)) ("0112"))
 #s(row "0021" (success immediate) ())
 #s(row "0022" (success immediate) ())
 #s(row "0100" (success (2 3)) ("0202" "0222"))
 #s(row "0200" (success (1 0)) ("2200" "1210" "1110"))
 #s(row "0101" (success (2 3)) ("0111"))
 #s(row "0102" (success (1 2)) ("0202" "0222"))
 #s(row "0201" (error no-bnds) ("0211" "2221" "2211" "2212" "2222"))
 #s(row "0202" (success (1 2)) ("0112"))
 #s(row "0110" (error no-bnds) ("2120" "2220" "2211" "2221" "2211" "2221" "2222"))
 #s(row "0120" (success (2 0)) ("0220" "0121" "1111"))
 #s(row "0210" (success (1 3)) ("0111"))
 #s(row "0220" (success (1 2)) ("0121" "0221"))
 #s(row "0111" (success immediate) ())
 #s(row "0112" (success immediate) ())
 #s(row "0121" (success (2 0)) ("1121" "1122" "1112"))
 #s(row "0122" (success (1 2)) ("0112"))
 #s(row "0211" (success (1 2)) ("0111"))
 #s(row "0212" (success (2 3)) ("0222"))
 #s(row "0221" (success immediate) ())
 #s(row "0222" (success immediate) ())
 #s(row "1000" (success (2 3)) ("1010" "1022"))
 #s(row "2000" (error no-bnds) ("2020" "2022" "2222"))
 #s(row "1001" (success immediate) ())
 #s(row "1002" (success immediate) ())
 #s(row "2001" (success (2 0)) ("1101" "1111"))
 #s(row "2002" (success (1 2)) ("2101" "1101" "1122" "1222"))
 #s(row "1010" (error no-bnds) ("1220" "1121" "2121" "2122" "2121" "2111" "2121" "2221" "2222"))
 #s(row "1020" (error no-bnds) ("1220" "1210" "2210" "2220" "2121" "2122" "2222"))
 #s(row "2010" (success (1 0)) ("2110" "1110"))
 #s(row "2020" (error no-bnds) ("2220" "2211" "2221" "2222"))
 #s(row "1011" (success (1 2)) ("1221"))
 #s(row "1012" (success (1 0)) ("1112"))
 #s(row "1021" (success immediate) ())
 #s(row "1022" (success immediate) ())
 #s(row "2011" (success (1 0)) ("1111"))
 #s(row "2012" (error no-bnds) ("2222"))
 #s(row "2021" (success (1 2)) ("2022" "1122" "1112"))
 #s(row "2022" (success (1 0)) ("2121" "2111" "1111"))
 #s(row "1100" (success (1 2)) ("1110"))
 #s(row "1200" (success (1 0)) ("2220" "2121" "2111" "1111"))
 #s(row "2100" (success (2 0)) ("2120" "2110" "2120" "2121" "2111" "1111"))
 #s(row "2200" (success (1 2)) ("1210" "1110"))
 #s(row "1101" (success (2 3)) ("1111"))
 #s(row "1102" (success (1 2)) ("1202" "1112"))
 #s(row "1201" (success (2 3)) ("1202" "1222"))
 #s(row "1202" (success (1 2)) ("1112"))
 #s(row "2101" (success (1 0)) ("2221" "2121" "2111" "1111"))
 #s(row "2102" (error no-bnds) ("2202" "2211" "2221" "2121" "1121" "2121" "2221" "2211" "2111" "2211" "2221" "2222"))
 #s(row "2201" (error no-bnds) ("2202" "2222"))
 #s(row "2202" (error no-bnds) ("2222"))
 #s(row "1110" (success immediate) ())
 #s(row "1120" (success (2 0)) ("1122" "1112"))
 #s(row "1210" (success (1 0)) ("1212" "1112"))
 #s(row "1220" (error no-bnds) ("2220" "2222"))
 #s(row "2110" (error no-bnds) ("2212" "2222"))
 #s(row "2120" (error no-bnds) ("2110" "2210" "2212" "2222"))
 #s(row "2210" (success (1 2)) ("2212" "2112" "2122" "1122" "1112"))
 #s(row "2220" (error no-bnds) ("2222"))
 #s(row "1111" (success immediate) ())
 #s(row "1112" (success immediate) ())
 #s(row "1121" (success (1 2)) ("1221"))
 #s(row "1122" (success (1 2)) ("1222"))
 #s(row "1211" (success (1 3)) ("1111"))
 #s(row "1212" (success (2 3)) ("1222"))
 #s(row "1221" (success immediate) ())
 #s(row "1222" (success immediate) ())
 #s(row "2111" (success (2 0)) ("2121" "2111" "1111"))
 #s(row "2112" (success (1 0)) ("2111" "1111"))
 #s(row "2121" (error no-bnds) ("2221" "2211" "2221" "2222"))
 #s(row
    "2122"
    (success (1 3))
    ("2112"
     "2111"
     "2211"
     "2221"
     "2211"
     "2221"
     "2121"
     "2221"
     "2211"
     "2111"
     "2211"
     "2221"
     "2121"
     "2221"
     "2211"
     "2111"
     "2211"
     "2111"
     "2121"
     "2111"
     "2121"
     "1121"
     "1122"
     "2122"
     "2112"
     "2111"
     "2121"
     "2122"
     "1122"
     "1222"))
 #s(row "2211" (success (1 0)) ("2212" "2211" "2111" "1111"))
 #s(row "2212" (error no-bnds) ("2222"))
 #s(row "2221" (error no-bnds) ("2121" "2221" "2222"))
 #s(row "2222" (error no-bnds) ()))
