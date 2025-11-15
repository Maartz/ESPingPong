-module(config).
-export([
         get/0
        ]).

get() ->
  #{
    sta => [ {ssid, "Livebox-5780"} , {psk, "yn9LrJVZ2dRNMRSxf3" }],
    port => 6969,
    led => #{ pin => 2, flash_duration => 100 }
   }.
