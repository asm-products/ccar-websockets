
portNumber = 20000
server = listen(portNumber)
while true
  println("Waiting for connections on port : " * string(portNumber))
  conn = accept(server)
  println("Accepted connection :" * string(conn))
  @async begin
    try
      while true
        println("Waiting for text")
        line = "Echo " * readline(conn)
        write(conn,line)
      end
    catch err
      println("connection ended with error $err")
    end
  end
end