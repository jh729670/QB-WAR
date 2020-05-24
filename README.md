# QB-WAR
Attempt to calculate how much Wins a Quarterback is worth to his team

In this exercise, I wanted to calculate how many wins does a Quarterback contirubute to his team. Much of this is based off of Major League Baseball's WAR metric; Wins Above Replacement. There is no universal metric for Football in calcuting this number, so I wanted to see if I could create my own metric. I used QB data going back to 2014 and took the average results of a specific statistics including, Touchdown, Passing Yards, Rushing Yards, Completion Percentage, Wins, Turnovers and Performance Metric. I then subtracted the averages of of these statistics from every Quarterbacks statistics to get a strong approximation of what Quarterbacks perform the most for their team.

What I found was that for a QB with a WAR of zero is worth about 5 wins, while a QB with a WAR of 10 is worth 10 wins. Intuitively this makes sense, as better performing Quarterbacks will tend to be on teams who have a lot of wins. 
