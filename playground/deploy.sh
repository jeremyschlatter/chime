set -ex
VERSION=${TAG:-latest}
IMAGE=gcr.io/chime-playground/playground:$VERSION
docker build --tag $IMAGE --file playground/Dockerfile .
docker push $IMAGE
gcloud run deploy --platform managed playground --region us-west1 --image $IMAGE
