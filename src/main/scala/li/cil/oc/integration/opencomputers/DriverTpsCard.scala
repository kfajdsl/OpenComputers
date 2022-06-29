package li.cil.oc.integration.opencomputers

import li.cil.oc.Constants
import li.cil.oc.api
import li.cil.oc.api.driver.EnvironmentProvider
import li.cil.oc.api.network.EnvironmentHost
import li.cil.oc.common.{Slot, Tier}
import li.cil.oc.server.component
import net.minecraft.item.ItemStack

object DriverTpsCard extends Item {
  override def worksWith(stack: ItemStack) = isOneOf(stack,
    api.Items.get(Constants.ItemName.TpsCard))

  override def createEnvironment(stack: ItemStack, host: EnvironmentHost) =
    if (host.world != null && host.world.isRemote) null
    else new component.TpsCard(host)

  override def slot(stack: ItemStack) = Slot.Card

  object Provider extends EnvironmentProvider {
    override def getEnvironment(stack: ItemStack): Class[_] =
      if (worksWith(stack))
        classOf[component.TpsCard]
      else null
  }
}
