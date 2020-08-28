project = 'scott-davis-remote'
zone = 'us-central1-a'
account_key = 'scott-davis-remote-56e7dae929b7.json'

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = project,
           GCE_DEFAULT_ZONE = zone)

library(googleComputeEngineR)

googleComputeEngineR::gce_vm_stop()

googleComputeEngineR::gce_list_machinetype()
gce_list_instances()
gce_list_machinetype()

gce_vm_stop(vm)


gce_attach_disk('tiny-studio',source = 'https://www.googleapis.com/compute/v1/projects/scott-davis-remote/zones/us-central1-a/disks/scott-gce-disk')
vm = gce_vm_start('small-studio')
gce_get_instance(vm)

?gce_attach_disk()

gce_attach_disk('tiny-studio',autoDelete = F,deviceName = 'scott-gce_disk')
)

gce_list_disks()

gce_get_instance(vm)
vm
gce_attach_disk('tiny-studio',)
vm = gce_vm_start('tiny-studio',)
gce_make_disk("scott-gce-disk",sizeGb = 10)
gce_get_instance(vm)

gce_vm_stop('small-studio')

str(vm)

gce_vm('small-studio')

vm <- gce_vm(template = "rstudio",
             name = "tiny-studio",
             username = "tscott1", password = "KgArRj24351!",
             predefined_type = "n1-standard-2")


googleComputeEngineR::gce_vm_stop(vm)
