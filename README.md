# chess

Apache Spark application wrote in Scala that implements Minimax algorithm to play to the chess game. 
The purpose of the application is to experiment with the building and use of a Minimax search tree over a Apache Spark cluster.

The application expects a single parameter that defines the depth of the Minimax search tree to be used (default is 5). 
To get the moves of the user player and to show the chess board and the moves of the computer the application uses a very basic text interface via the standard console,
so you have to submit the application to Apache Spark in client mode to interact with it.

## Parallelism behaviour

- The first level of children is deployed by the driver and parallelized with a RDD object to the parallel tasks.
- The next levels are deployed by the same task associated to the parent of the first level.
- Once the Minimax values have been calculated by the parallel tasks the tree is pruned keeping only the first two levels (first Max level and first Min level).
- The tree pruned is shuffled to the driver.
- Once Max (computer) and Min (user) have moved continuous with the first step again.