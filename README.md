# elm-time

A web-app written in Elm to make tracking your time and entering timesheets easier. 

It turns this:
```
0900 Ticket 123
1000 Resolving Live issue
1100 Team Meeting
1200 Lunch
1230 Ticket 123
1400 Project Manager update
1415 Ticket 123
1500 QA for Ticket 124
1530 Ticket 125
1700 Home
```

into

* 0h 30m - Lunch
* 0h 15m - Project Manager update
* 0h 30m - QA for Ticket 124
* 1h 0m - Resolving Live issue
* 1h 0m - Team Meeting
* 3h 15m - Ticket 123
* 1h 30m - Ticket 125

[Try it yourself!](http://guess-burger.github.io/time_tracker.html)

## Goals

This web-app was written as a way to learn Elm and make something I'd use everyday. 

Theres no doubt that if you're employed then someone wants to know what you've spent your day doing!

I find keeping track of how long I've spent on a task and updating whatever tracking software your employer uses as you go along easier said than done.
Instead, I make a quick note of when I switch task so I can work out the times later.

The problem comes at the end of a long day when you were meant to have left 10 minutes ago and are faced a long list of times and tasks to calculate.
That's where elm-time comes in, simply make a note of when you start each task and it will do the maths for.

## TODO 
* Add a total time
** Filter out times for lunch/breaks in the total
* Add some style
* Task suggestion
* Save entered items in local storage
* Display JIRA decimal time values along with the more human readable "1h 15m" format
