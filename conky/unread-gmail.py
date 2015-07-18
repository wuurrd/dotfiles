import imaplib
import keyring
work = imaplib.IMAP4_SSL('imap.gmail.com','993')
work.login('david@pexip.com',
          keyring.get_password('gmail', 'david@pexip.com'))
work.select()
work.search(None,'UnSeen')
print len(work.search(None, 'UnSeen')[1][0].split())
