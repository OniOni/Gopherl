-module(gopherl).
-author("Mathieu Sabourin").

-export([listen/1, start/1, list_menu/0, parse_menu/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).

central({init, Menu}) ->
    Dict = dict:from_list(parse_menu(Menu)),
    central(Dict);

central(Dict) ->
    receive
	{get, Key, Pid} ->
	    Pid ! dict:fetch(Key, Dict);
	{all, Pid} ->
	    Pid ! dict:to_list(Dict)
    end,
    central(Dict).

start(Port) ->
    register(central, spawn(fun() -> central({init, "files"}) end)),
    listen(Port).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).


accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    inet:setopts(Socket, ?TCP_OPTIONS),
	    io:format("Accept~n"),
	    gen_tcp:controlling_process(Socket, spawn(fun() ->  loop(Socket) end)),
	    accept(LSocket);
	Other ->
	    io:format("Got [~w]~n", [Other]),
	    ok
    end.

file_t_to_str(T) ->
    case T of
	{Name, directory} ->
	    (((("1" ++ Name) ++ "\t") ++ Name) ++ "\tlocalhost\t70\r\n");
	{Name, regular} ->
	    (((("0" ++ Name) ++ "\t") ++ Name) ++ "\tlocalhost\t70\r\n")
    end.

process_file_data(Data) ->
    case Data of
	[H | []] ->
	    file_t_to_str(H) ++ ".\r\n";
	[H | T] ->
	   file_t_to_str(H) ++ process_file_data(T)
    end.	
    
parse_menu(Menu) ->
    lists:map(fun(File) -> Dir = Menu ++ "/", 
				 case file:read_file_info(Dir ++ File) of
				     {ok, Data} ->
					 {File, element(3, Data)};
				     Other -> {error, Other}
				 end
	      end, 
	      element(2, file:list_dir(Menu))).
	    

list_menu() ->
    central ! {all, self()},
    receive 
	List ->
	    io:format("Received~n"),
	    process_file_data(List)
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
	    case bitstring_to_list(Data) of
		[13, 10] -> %Receive CR LF
		    io:format("Got CR LF~n"),
		    gen_tcp:send(Socket, 
				 list_menu());
		Other ->
		    io:format("Got ~w~n", [Other])
	    end		
    end.

