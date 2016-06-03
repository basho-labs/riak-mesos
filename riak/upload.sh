#!/bin/bash

if [ -z "$OAUTH_TOKEN" ]; then
    export OAUTH_TOKEN=$(cat oauth.txt)
fi
if [ -z "$RIAK_MESOS_TAG" ]; then
    export RIAK_MESOS_TAG=$(git describe --tags --abbrev=0)
    # export RIAK_MESOS_TAG=1.0.0
fi
if [ -z "$RELEASE_ID" ]; then
    export RELEASE_ID=$(curl -sS https://api.github.com/repos/basho-labs/riak-mesos/releases/tags/$RIAK_MESOS_TAG?access_token=$OAUTH_TOKEN | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
fi
if [ -z "$OS_FAMILY" ]; then
    export OS_FAMILY="ubuntu"
fi
if [ -z "$OS_VERSION" ]; then
    export OS_VERSION="14.04"
fi
if [ -z "$RIAK_TYPE" ]; then
    export RIAK_TYPE="riak"
fi
if [ -z "$RIAK_VERSION" ]; then
    export RIAK_VERSION="2.1.4"
fi
if [ -z "$RIAK_PKGNAME" ]; then
    export RIAK_PKGNAME="$RIAK_TYPE-$RIAK_VERSION-$OS_FAMILY-$OS_VERSION.tar.gz"
fi
if [ -z "$DEPLOY_BASE" ]; then
    export DEPLOY_BASE="https://uploads.github.com/repos/basho-labs/riak-mesos/releases/$RELEASE_ID/assets?access_token=$OAUTH_TOKEN&name=$RIAK_PKGNAME"
fi
if [ -z "$DOWNLOAD_BASE" ]; then
    export DOWNLOAD_BASE="https://github.com/basho-labs/riak-mesos/releases/download/$RIAK_MESOS_TAG/$RIAK_PKGNAME"
fi

echo "Uploading to "$DOWNLOAD_BASE
curl -sS -XPOST -H 'Content-Type: application/gzip' "$DEPLOY_BASE" --data-binary @$RIAK_PKGNAME
curl -sS -XPOST -H 'Content-Type: application/octet-stream' "$DEPLOY_BASE.sha" --data-binary @$RIAK_PKGNAME.sha
