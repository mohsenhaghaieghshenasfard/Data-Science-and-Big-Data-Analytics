

FROM debian
MAINTAINER Docker Workshop <mohsen@gwdg.de>

RUN apt-get update
RUN apt-get install -y nginx
RUN echo 'HI, I am in your container' \
    > /usr/share/nginx/html/index.html

CMD [ "nginx", "-g", "daemon off;"]

EXPOSE 80
