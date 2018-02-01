# Cover-Your-Assets
Recreating Grandpa Beck's card game Cover Your Assets in OpenEdge's language Progress 4GL

Game Actions:

Discard and draw                                  DONE

Pick-up card from discard pile and make stack     IN PROGRESS

Match 2 cards in hand and make stack

Challenge Player

  challenge and response cadence
  
  draw n cards after challenge
  
  
First iteration will just be proof of concept, possible one player game with AI.  Really just want to make sure the logic is there.
Then, add functionality for multiple players to connect.  I'm thinking of creating a DB and making a table for the deck, adding a batch-id field (in leu of current temp-table solution).  Players could connect and reference batch-id of deck table to be in the same game.  Game engine could include queries of deck table to play the game accross multiple computers.
