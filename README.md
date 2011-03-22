From Zero to Emonk
==================

Code and notes for my Erlang Factory talk.

Outline
-------

0. This talk
1. What is a NIF?
2. Your first NIF
  a. Code sample
  b. Output
3. Simple term manipulations
  a. Creating terms
  b. Testing terms
  c. Getting terms from argv
4. Resource Objects
  a. What are they?
  b. How can we use them?
  c. Pointers!
    i. Code sample
    ii. Output
  d. State mutation!
    i. Code sample
    ii. Output
5. Environments
  a. What are they?
  b. Why do we need them?
6. Message Passing
  a. Sending terms
    i. Code sample
    ii. Output
7. Threading
  a. Yes threads
  b. Thin wrapper around pthreads
  c. term\_sending
    i. Code sample
    ii. Output
  d. queue
    i. Code sample
    ii. Output
9. Emonk - What is it?
  a. JavaScript (YANEJE)
  b. Erlang API
  c. JavaScript API
10. SpiderMonkey - Runtime, Context, Go
11. Emonk - Architecture
  a. Big picture
12. Emonk - NIF API Usage
  a. Term conversion 
  b. Resources
  c. Threads
  d. Message passing
