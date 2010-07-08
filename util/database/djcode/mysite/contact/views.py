from django.core.mail import send_mail
from django.http import HttpResponseRedirect
from django.shortcuts import render_to_response

def contact(request):

    errors= []
    if request.method=='POST':
       if not request.POST.get('subject',''):
          errors.append('Enter a subject.')
    if not request.POST.get('message',''):
       errors.append('Enter a message.')
    if request.POST.get('e-mail') and '@' not in request.POST['e-mail']:
       errors.append('Enter a valid e-mail address.')
    if not errors:
       send_mail(
            request.POST['subject'],
            request.POST['message'],
            request.POST.get('e-mail','noreply@example.com'),
                             ['siteowner@example.com'],
       )
       return HttpResponseRedirect('/contact/thanks/')
    return render_to_response('contact_form.html',
                              {'errors': errors})
 )
