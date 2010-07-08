from django.http import HttpResponse
from django.template.loader import get_template
from django.shortcuts import render_to_response
#from mysite.books.models import Book
from django.template import Context
import datetime

def hello(request):
    return HttpResponse("Hello world")

def current_datetime(request):
    now=datetime.datetime.now()
    t=get_template('current_datetime.html')
    html=t.render(Context({'current_date':now}))
    return  HttpResponse(html)

def hours_ahead(request,offset):
    try:
       offset=int(offset)
    except ValueError:   
       raise Http404()
    dt=datetime.datetime.now() + datetime.timedelta(hours=offset)
    html="<html><body>In %s hour(s),it will be %s.</body></html>" % (offset,dt)
    return HttpResponse(html)

def search_form(request):
    return render_to_response('search_form.html')

#def search(request):
#    error=False
#    if 'q' in request.GET: 
#        q=request.GET['q']
#        if not q:
#           errors.append('Enter a search term.')
#        elif len(q)>20:
#           errors.append('Please enter at most 20 characters.')
#        else:
#           books=Book.object.filter(title_icontains=q)#lista de libros cuyo titulo tenga "q"
#           return reder_to_response('search_results.html'),
#                                    {'books':books,'query':q})
#    return render_to_response('search_form.html',{'error':error})




