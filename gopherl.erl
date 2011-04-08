-module(gopherl).
-author("Mathieu Sabourin").

-export([listen/1, start/1, list_menu/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).

start(Port) ->
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
    
	    

list_menu(Menu) ->    
    Tmp = lists:map(fun(File) -> Dir = Menu ++ "/", 
			   case file:read_file_info(Dir ++ File) of
			       {ok, Data} ->
				   {File, element(3, Data)};
			       Other -> {error, Other}
			   end
	      end, 
	      element(2, file:list_dir(Menu))),
    process_file_data(Tmp).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
	    case bitstring_to_list(Data) of
		[13, 10] -> %Receive CR LF
		    io:format("Got CR LF~n"),
		    gen_tcp:send(Socket, 
				 list_menu("files"));
		Other ->
		    io:format("Got ~w~n", [Other])
	    end		
    end.

