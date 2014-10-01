/*
Copyright 2014 sumito3478 <sumito3478@gmail.com>
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package scarango
package http

import com.ning.http.client.providers.netty.NettyAsyncHttpProviderConfig
import scala.concurrent._
import com.ning.http.client._
import org.jboss.netty.util._
import org.jboss.netty.channel.socket.nio._
import java.util.{ concurrent => juc }

object Executor {
  private lazy val (providerConfig, threadPool) = {
    // Setup the underlying Netty Provider to:
    //   1) Use daemon threads
    //   2) Release resources at interruption
    lazy val threadFactory: juc.ThreadFactory = new juc.ThreadFactory {
      val once = new juc.atomic.AtomicBoolean(false)
      def newThread(runnable: Runnable) = new Thread(runnable) {
        setDaemon(true)
        override def interrupt() = {
          if (once.compareAndSet(false, true)) {
            channelFactory.releaseExternalResources()
            timer.stop()
          }
          super.interrupt()
        }
      }
    }
    lazy val threadPool: juc.Executor = juc.Executors.newCachedThreadPool(threadFactory)
    lazy val timer = new HashedWheelTimer(threadFactory)
    lazy val channelFactory: NioClientSocketChannelFactory = new NioClientSocketChannelFactory(threadPool, 1,
      new NioWorkerPool(threadPool, Runtime.getRuntime.availableProcessors * 2), timer)
    val providerConfig = new NettyAsyncHttpProviderConfig().addProperty(NettyAsyncHttpProviderConfig.SOCKET_CHANNEL_FACTORY, channelFactory)
    providerConfig.setNettyTimer(timer)
    (providerConfig, threadPool)
  }
}

import Executor._

case class ExecutorOptions(userAgent: String = s"Mozilla/5.0 (compatible; scarango/${BuildInfo.version}}; +https://github.com/sumito3478/scarango)")

case class Executor(options: ExecutorOptions = ExecutorOptions()) extends Disposable {
  lazy val client = new AsyncHttpClient(new AsyncHttpClientConfig.Builder()
    .setAsyncHttpClientProviderConfig(providerConfig)
    .setUserAgent(options.userAgent)
    .setRequestTimeoutInMs(-1).build)
  def apply[A](request: Request, handler: AsyncHandler[A]): Future[A] = {
    val jfuture = client.executeRequest(request, handler)
    val p = Promise[A]()
    jfuture.addListener(new Runnable {
      def run = p.complete(scala.util.Try(jfuture.get))
    }, threadPool)
    p.future
  }
  override def disposeInternal() = client.close()
}
