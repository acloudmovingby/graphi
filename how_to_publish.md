#
All necessary passwords/tokens are in Google Password Manager
You can login to Sonatype website using my Github account to see published artifacts: https://central.sonatype.com/publishing

# Shell commands

Do this:
```shell
export MILL_SONATYPE_USERNAME=<in Google Password Manager> # under sonatype.com, the one that starts with 'y'
export MILL_SONATYPE_PASSWORD=<in Google Password Manager>
export MILL_PGP_SECRET_BASE64=<in Google Password Manager> # under "pgp%20secret.com"
export MILL_PGP_PASSPHRASE=<in Google Password Manager> 

./mill graphi.publishSonatypeCentral
```
