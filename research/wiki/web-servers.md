# web servers[⌁](https://rdf.lodgeit.net.au/kb/web_servers)

we chose django because of familiarity but this comparison is worth reading. Also one of the alternative frameworks i think supports a linked data backend.
https://www.airpair.com/python/posts/django-flask-pyramid

if we need to integrate django with twisted in future, there are two projects that accomplish this: https://github.com/hendrix/hendrix https://github.com/itamarst/crochet

The plan as of 10/2020 is to migrate the django stuff to FastAPI. FastAPI has a complete wsgi/docker deployment story, as opposed to django. It also generates swagger docs out of the box.

python server is the public-facing gateway, and spawns a fresh prolog process for each request. If needed, prolog could run persistently and dispatch requests from a queue, but this solution is goood for debugging aed development.

After some discussion, we decided not to pursue aws lambda and similar solutions, but stick to a traditional vm with a permanent python server process.


