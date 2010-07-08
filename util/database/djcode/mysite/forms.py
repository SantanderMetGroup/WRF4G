from django import forms

class ContactForm(forms.Form):

   subject=forms.CharField()
  # e-mail=forms.EmailField() 
   message=forms.CharField()


