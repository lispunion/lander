(ansible

 (options)

 (groups
  (group
   (name hello-hosts)
   (hosts
    (host
     (name localhost)
     (vars
      (var ansible-host "localhost"))))))

 (playbooks
  (playbook
   (name hello)
   (hosts hello-hosts)
   (become true)
   (roles
    write-hello-world)))

 (roles

  (role
   (name write-hello-world)
   (tasks
    (task
     (title "write a hello world text file to /tmp")
     (copy
      (dest "/tmp/hello-world.text")
      (content "The alien has landed")))))))
