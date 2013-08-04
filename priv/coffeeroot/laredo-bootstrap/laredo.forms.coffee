root = exports ? this

class LaredoObject
      @laredo: {}

class LaredoPost
      log -> console.log ("howdy")

unless root.LAREDO
       root.LAREDO = new LaredoObject

L = root.LAREDO

L.post = new LaredoPost
L.post.log()