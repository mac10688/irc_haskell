# Server Architecture
The server comes in two parts for now.

- Main.hs
- Shared-library

As it stands there is too much logic in Main.hs and the module should be split up but that will be for the future.
The shared-library mainly houses the payload information to be communicated between client and server.

The payload can be split into 3 categories.

1. Requests - the client can send request to the server.
2. Responses - the server may respond to the client individually to the request.
3. Broadcasts - the server may also respond to a request by sending out scoped

In essence, a client will send a request to the server and the server will most likely send back a response to the client directly and a different broadcast to the other users. The only time that a response is not given to the client is sending a message to a room.

# Building/Running The Server

> `cabal build`

> `cabal run irc-server-exe`

# A summary of server endpoints:

## Login:

### Client Request

> `{“request”:”LOGIN”,“username”:<username>}`

> `<username>` is the handle the user wishes to assign himself. The name must not contain punctuation, whitespace nor be empty.

### Server Response
> `{“response”:”USER_LOGGED_IN”, “userId”:<userId>}`

> `<userId>` is the UUID assigned to the user upon successful login.
---
## Create A Room:

The client will be able to create a room for others to join.

### Client Request
> `{“request”:”CREATE_ROOM”,“room_name”:<roomname>}`
	
> `<roomname>` will be the name the user wishes to assign to the room. There are no checks on room name.

### Server Response
> `{“response”:”ROOM_CREATED”, “roomId”:<roomId>}`

> `<roomId>` is the UUID assigned to the room for the client to keep track of.
---
## Destroy Room
A user can destroy a chat room and all users in that room will be kicked out of the room.

### Client Request
> `{“request”:”DESTROY_ROOM”, “roomId”:<roomId>}`

### Server Response

> `{“response”:”ROOM_DESTROYED”, “roomId”:<roomId>}`

> `<roomId>` the id of the room that was destroyed for confirmation.

### Broadcast Response

> `{“broadcast”:”ROOM_DESTROYED”,”roomId”:<roomId>}`

> `<roomId>` the id of the room that was destroyed.
---
## Join A Room:

If a room exists, a client can join that room.

### Client Request
> `{“request”:”JOIN_ROOM”,“roomId”:<roomId>}`

> `<roomId>` will be the id of the room the client received either upon creating a room or getting a list of rooms.

### Server Response
> `{“response”:”ROOM_JOINED”, “roomId”:<roomId>, “users”:<users>}`

> `<roomId>` is returned so the user can know which room they were allowed to join.

> `<users>` are the list of users that are also in the room.

### Broadcast Response
> `{“broadcast”:”USER_JOINED_ROOM”, “roomId”:<roomId>, “userId”:<userId>, “username”:<username> }`

> `<roomId>` is the id of the room the user is joining.

> `<userId>` is the id of the user who is joining the room.

> `<username>` is the username of the user who is joining.
---
## Leave A Room:

A client will have the option to leave a room to stop receiving broadcasts directed to users of a room.

### Client Request
> `{“request”:”LEAVE_ROOM”,“roomId”:<roomId>}`

> `<roomId>` will be the id of the room the client received either upon creating a room or getting a list of rooms.

### Server Response
> `{“response”:”ROOM_LEFT”, “roomId”:<roomId>}`

> `<roomId>` is returned so the user can know which room they were allowed to join.

### Broadcast Response

> `{“broadcast”:”USER_LEFT_ROOM”,”roomId”:<roomId>,”userId”:<userId>}`

> `<roomId>` is the id of the room that the user is leaving.

> `<userId>` the userId that left the room.
---
## List All Rooms:

A user will have ability to request all the rooms they can join.

### Client Request
> `{“request”:”LIST_ALL_ROOMS”}`

### Server Response
> `{“response”:”LIST_OF_ROOMS”,“rooms”:<rooms>}`

> `<rooms>` will be a list of objects of rooms that include roomId and room name.
---
## List Room Members:

A user will have the ability to see everyone in the room after they join.

### Client Request

> `{“request”:”LIST_ROOM_MEMBERS”, “roomId”:<roomId>}`

> `<roomId>` will be the id of the room the client received either upon creating a room or getting a list of rooms.

### Server Response

> `{“response”:”LIST_OF_USERS”,”roomId”:”<roomId>”,“users”:<users>}`

> `<roomId>` is the roomId associated with the list of users. The users are structured JSON objects containing an Id and the username.
---
## Send A Message To Room:

A user can send a message targeted to a room which will be broadcasted to everyone else in the room.

### Client Request

> `{“request”:”SEND_ROOM_MSG”, “roomId”:<roomId>, “data”:<data>}`

> `<roomId>` is id of the room the client wants to send a message to.

> `<data>` is the msg data the room will receive.

### Server Broadcast

> `{“broadcast”:”ROOM_MESSSAGE”, “roomId”:<roomId>, “userId”:”<userId>”, “username”:”<username>”,“msg”:<msg>}`

> `<roomId>` to let client know the room that is targeted.

> `<userId>` is the user who sent the msg

> `<username>` a convenience field to let the client know the username of the sender.

> `<msg>` is the message the user sent.
---
## Logout:
To logout a user will send a logout message to the server and the server will close the connection.

### Client Request
> `{“request”:”LOGOUT”}`

### Server Response

> `{“response”:”USER_LOGGED_OUT”, “userId”:<userId>}`

> `<userId>` isn’t necessary for this response but it’s returned anyway. Just for extra confirmation.

### Broadcast Response

> `{“broadcast”:”USER_LOGGED_OUT”,”userId”:<userId>,”rooms”:[<roomId>]}`

> `<userId>` the id of the user who has logged out.

> `<roomId>` the room the user was in. The client can remove the user based on this notification.

# Client

The client can be located at this github repository. [That repository can be found here.](https://github.com/t1mel0rd42/CS-594-Project/tree/wtf-)

This project is written in Python with no respect to functional programming. It's author is Darin Kohn for CS 594.

In Adv. Functional Programming I will look to write the UI in Haskell using Monero.