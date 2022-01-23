# Arbitrage

It's simple script which takes currency rates from https://fx.priceonomics.com/v1/rates/
The result contains following information:

1. If script was started with `arbitrage ${currency}` then there will be information
   if there is an arbitrage opportunity from that currency
2. Each time algorithm is going to show also other arbitrage opportunities

# Running

Script can be started:

1. With `currency` parameter: <br>
   `amm Arbitrage.sc arbitrage usd`
2. To detect all arbitrage possibilities <br>
   `amm Arbitrage.sc arbitrageAll`
3. To run simple tests <br>
   `amm tests/Tests.sc`

# TODOs

There are quite a lot to improve.

1. First of all there is no input validation <br>
   a. if the currencies won't be an square matrix it would fail <br>
   b. Of course parsing currency keys is based on `split` method
   c. Api failure will result in script failure with exception
2. Algorithm updagrades <br>
   a. probably there should be also a possibility to stop looking for arbitrage when first one has been found - should be considered if it will be an performance improvment <br>
   b. There are also others than Bellman Ford algorithms. Ex. https://en.wikipedia.org/wiki/Triangular_arbitrage may lead to quicker execution on small matrixes <br>
   c. memory usage - if neccessary there is option to move even to neighboor lists and express it as Array[Array[]] - should be taken into account that readability drops <br>
   d. during research I've found that there are also some upgrades on Bellman Ford algorithm such as https://pragmaticprogramming.wordpress.com/2017/04/25/bellman-ford-shortest-path-algorithm-optimization/ which may lead to quicker execution due to reduction of relaxation loops

# Complexity

As it is a standard Bellman Ford algorithm it runs in <br>
`O(VE)` in that situation as currency matrix is square matrix it would be `O(n^3)` due to fact that E is a dense graph `O(n^2)`
