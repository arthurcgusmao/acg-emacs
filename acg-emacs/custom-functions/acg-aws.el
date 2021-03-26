(defun acg/aws-source-credentials ()
  "Source AWS credentials from file."
  (interactive)
  (source "~/.aws/aws-mfa-login-credentials"))
