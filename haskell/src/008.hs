{-
 - Project Euler 8
 -
 - Find the greatest product of five consecutive digits in the 1000-digit number.
 -
 - 73167176531330624919225119674426574742355349194934
 - 96983520312774506326239578318016984801869478851843
 - 85861560789112949495459501737958331952853208805511
 - 12540698747158523863050715693290963295227443043557
 - 66896648950445244523161731856403098711121722383113
 - 62229893423380308135336276614282806444486645238749
 - 30358907296290491560440772390713810515859307960866
 - 70172427121883998797908792274921901699720888093776
 - 65727333001053367881220235421809751254540594752243
 - 52584907711670556013604839586446706324415722155397
 - 53697817977846174064955149290862569321978468622482
 - 83972241375657056057490261407972968652414535100474
 - 82166370484403199890008895243450658541227588666881
 - 16427171479924442928230863465674813919123162824586
 - 17866458359124566529476545682848912883142607690042
 - 24219022671055626321111109370544217506941658960408
 - 07198403850962455444362981230987879927244284909188
 - 84580156166097919133875499200524063689912560717606
 - 05886116467109405077541002256983155200055935729725
 - 71636269561882670428252483600823257530420752963450
 -}
module Euler8 (euler8) where
import Util(intToList)
import Numbers(groupConsecDigits)

euler8 n = maximum . map product $ groupConsecDigits 5 n

answer = euler8 n where
    n =intToList (read "731671765313306249192251196744265747423\
        \553491949349698352031277450632623957831801698480\
        \1869478851843858615607891129494954595017379583319\
        \5285320880551112540698747158523863050715693290963\
        \29522744304355766896648950445244523161731856403098\
        \71112172238311362229893423380308135336276614282806\
        \44448664523874930358907296290491560440772390713810\
        \51585930796086670172427121883998797908792274921901\
        \69972088809377665727333001053367881220235421809751\
        \25454059475224352584907711670556013604839586446706\
        \32441572215539753697817977846174064955149290862569\
        \32197846862248283972241375657056057490261407972968\
        \65241453510047482166370484403199890008895243450658\
        \54122758866688116427171479924442928230863465674813\
        \91912316282458617866458359124566529476545682848912\
        \88314260769004224219022671055626321111109370544217\
        \50694165896040807198403850962455444362981230987879\
        \92724428490918884580156166097919133875499200524063\
        \68991256071760605886116467109405077541002256983155\
        \20005593572972571636269561882670428252483600823257\
        \530420752963450" ::Integer )

