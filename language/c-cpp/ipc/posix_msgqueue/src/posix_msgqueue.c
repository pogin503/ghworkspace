#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <mqueue.h>
#include <stdio.h>
#include <string.h>

#define QUEUE_NAME "/queue1"

int main(void) {
  mqd_t msg_queue_id;
  unsigned int priority = 0;
  char msg[100];
  int status;

  msg_queue_id = mq_open(QUEUE_NAME, O_RDWR | O_CREAT | O_EXCL, S_IRWXU | S_IRWXG, NULL);
  if (msg_queue_id == -1) {
      perror("Unable to create queue");
      return 2;
  }

  strcpy(msg, "Hello world!");

  status = mq_send(msg_queue_id, msg, strlen(msg)+1, priority);
  if (status == -1) {
      perror("Unable to send message");
      return 2;
  }

  status = mq_close(msg_queue_id);
  if (status == -1) {
      perror("Unable to close message queue");
      return 2;
  }

  return 0;
}
