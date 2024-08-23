all: docker

FILE=simpsych-example

## build docker image (requires root rights for docker)
dbuild: Dockerfile
	docker build \
    -t $(FILE) .

## run docker
docker: dbuild
	echo "open RStudio Server at http://localhost:8787"
	docker run \
    --rm \
	-ti \
	-e DISABLE_AUTH=true \
	-e ROOT=true \
	-e USERID=$(id -u) \
	-e GROUPID=$(id -g) \
	-p 8787:8787 \
	-v $(CURDIR)/simulation-example:/home/rstudio/simulation-example \
	-v $(CURDIR)/figures:/home/rstudio/figures \
	$(FILE)
