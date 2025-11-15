-module(iot_node).
-export([start/0]).

start() ->
    io:format("~n=== Ping Pong Starting ===~n~n"),

    case atomvm:platform() of
        esp32 ->
            init_led(),
            io:format("Connecting to WiFi...~n"),
            start_wifi();
        Platform ->
            io:format("Unsupported platform: ~p~n", [Platform]),
            {error, {unsupported_platform, Platform}}
    end.

init_led() ->
    Config = config:get(),
    LedConfig = maps:get(led, Config),
    Pin = maps:get(pin, LedConfig),
    
    gpio:set_pin_mode(Pin, output),
    gpio:digital_write(Pin, low),
    
    io:format("LED initialized on pin ~p~n", [Pin]).

flash_led() ->
    Config = config:get(),
    LedConfig = maps:get(led, Config),
    Pin = maps:get(pin, LedConfig),
    Duration = maps:get(flash_duration, LedConfig),
    
    gpio:digital_write(Pin, high),
    timer:sleep(Duration),
    gpio:digital_write(Pin, low).

start_wifi() ->
    Config = config:get(),
    StaConfig = maps:get(sta, Config),
    
    case network:wait_for_sta(StaConfig, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format("IP: ~p~n", [Address]),
            io:format("Netmask: ~p~n", [Netmask]),
            io:format("Gateway: ~p~n", [Gateway]),
            start_distribution(Address);
        Error ->
            Error
    end.

start_distribution(Address) ->
    {ok, _EPMDPid} = epmd:start_link([]),
  
    {X, Y, Z, T} = Address,
    Node = list_to_atom(lists:flatten(io_lib:format("atomvm@~B.~B.~B.~B", [X, Y, Z, T]))),
    
    {ok, _NetKernelPid} = net_kernel:start(Node, #{name_domain => longnames}),
    net_kernel:set_cookie(<<"AtomVM">>),
    io:format("Cookie: ~s~n~n", [net_kernel:get_cookie()]),
    
    io:format("~nDistribution started successfully!~n"),
    io:format("Node name: ~p~n", [node()]),
    
    
    OtherNodeAddress = case T of
        50 -> "atomvm@192.168.1.49";
        49 -> "atomvm@192.168.1.50";
        _ -> "atomvm@192.168.1.49"
    end,
    OtherNode = list_to_atom(OtherNodeAddress),
    
    io:format("This node will ping-pong with: ~p~n", [OtherNode]),
    io:format("~nTo start the ping-pong, from an Erlang shell run:~n"),
    io:format("erlang:set_cookie('~s', 'AtomVM').~n", [Node]),
    io:format("{ping_pong, '~s'} ! ping.~n~n", [Node]),

    register(ping_pong, self()),

    io:format("Waiting 5 seconds before starting ping-pong...~n"),
    timer:sleep(5000),

    case T of
        50 ->
            io:format("I'm .50, sending initial PING to start the game!~n"),
            {ping_pong, OtherNode} ! ping;
        _ ->
            io:format("I'm .49, waiting for the first ping...~n")
    end,

    ping_pong_loop(OtherNode).

ping_pong_loop(OtherNode) ->
    receive
        ping ->
            io:format("[~s] Received PING! Flashing LED and sending PONG...~n", 
                     [format_time()]),
            flash_led(),
            
            timer:sleep(500),
            
            {ping_pong, OtherNode} ! pong,
            io:format("[~s] PONG sent to ~p~n~n", [format_time(), OtherNode]),
            
            ping_pong_loop(OtherNode);
            
        pong ->
            io:format("[~s] Received PONG! Flashing LED and sending PING...~n", 
                     [format_time()]),
            flash_led(),
            
            timer:sleep(500),
            
            {ping_pong, OtherNode} ! ping,
            io:format("[~s] PING sent to ~p~n~n", [format_time(), OtherNode]),
            
            ping_pong_loop(OtherNode);
            
        stop ->
            %% TODO: not stop forever but just be idle (check deep sleep)
            io:format("Ping-pong stopped.~n"),
            ok;
            
        Other ->
            io:format("Unexpected message: ~p~n", [Other]),
            ping_pong_loop(OtherNode)
    end.

format_time() ->
    Millis = erlang:system_time(millisecond),
    Seconds = Millis div 1000,
    Ms = Millis rem 1000,
    io_lib:format("~B.~3..0Bs", [Seconds, Ms]).
