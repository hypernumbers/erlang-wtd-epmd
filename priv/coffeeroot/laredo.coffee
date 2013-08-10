# Coffeescript call backs

# create a master object on the window and hang this class on it
root = exports ? this

# make jQuery a local alias
$ = jQuery

class LaredoObject
      @laredo: {}

class LaredoLongPoll

      poll_server = ->
           console.log "polling"
           setTimeout poll_server, 1000


      poll: ->
            poll_server()

L.longPoll = new LaredoLongPoll
L.longPoll.poll()