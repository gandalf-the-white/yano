USER_DOCKER 	?=${TF_VAR_docker_login}
IMAGES			?= cl-frontend cl-proxy cl-backend cl-oracle
PLATFORM		?= linux/amd64
TAG      		?= latest
DOCKERFILE_DIR 	?= docker
DEPLOY			?= namespace frontend backend oracle gateway httproute
UNDEPLOY		?= httproute gateway oracle backend frontend namespace

.PHONY: all build push clean check-user

all: build

check-user:
	@if [ -z "$(USER_DOCKER)" ]; then \
		echo "ERROR: USER_DOCKER is empty. Set TF_VAR_docker_login or run: make USER=<dockerhub_user>"; \
		exit 1; \
	fi

build: check-user
	@set -e; \
	for img in $(IMAGES); do \
		echo "==> Building $(USER_DOCKER)/$$img:$(TAG)"; \
		docker build --platform "$(PLATFORM)" \
			-t "$(USER_DOCKER)/$$img:$(TAG)" \
			-f "$(DOCKERFILE_DIR)/Dockerfile.$$img" . ; \
	done

push: check-user
	@set -e; \
	for img in $(IMAGES); do \
		echo "==> Pushing $(USER_DOCKER)/$$img:$(TAG)"; \
		docker push "$(USER_DOCKER)/$$img:$(TAG)"; \
	done

deploy:
	@set -e; \
    for img in $(DEPLOY); do \
		echo "==> Deploy $$img.yml"; \
        kubectl apply -f "manifests/$$img.yml" ; \
    done

undeploy:
	@set -e; \
    for img in $(UNDEPLOY); do \
		echo "==> Undeploy $$img.yml"; \
        kubectl delete -f "manifests/$$img.yml" ; \
    done

clean:
	docker system prune -f
