# create a master object on the window and hang this class on it
root = exports ? this

# make jQuery a local alias
$ = jQuery

class LaredoObject
      @laredo: {}

class LaredoPost

      # define some internal fns
      click = (e) ->
            e.preventDefault()
            Id = e.currentTarget.parentNode.parentNode.id
            Selector = "#" + Id + " input"
            Inputs = $(Selector)
            console.log(Inputs)

      # define the api which we will return
      # this is the last statement so it is returned automatically
      api:
           bind: ->
            $('.laredo-submit').click(click)


unless root.LAREDO
       root.LAREDO = new LaredoObject

L = root.LAREDO

L.post = new LaredoPost
L.post.api.bind()