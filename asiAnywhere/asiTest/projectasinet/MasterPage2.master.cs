using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class MasterPage2 : System.Web.UI.MasterPage
{
    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();


        if (fname == "corrugated_brow_aspx")
        {
            librwsjobs.Attributes.Add("class", "selected");
        }
        else if (fname == "jobprod_aspx")
        {
            liviewjob.Attributes.Add("class", "selected");
        }
        else if (fname == "material_aspx")
        {
            limaterial.Attributes.Add("class", "selected");
        }
        else if (fname == "materialinfo_aspx")
        {
            limatlinfo.Attributes.Add("class", "selected");
        }
        else if (fname == "machhrs_aspx")
        {
            limachhrs.Attributes.Add("class", "selected");
        }
        else if (fname == "machqty_aspx")
        {
            limachqtys.Attributes.Add("class", "selected");
        }
        else 
        {
            liwaste.Attributes.Add("class", "selected");
        }


        if (Session["show"] != null)
        {
            librwsjobs.Visible = true;
        }
        else
        {
            librwsjobs.Visible = false;

        }
        
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

                string vUserIdviewjob = UserLogin.UserName;
                string vPageviewjob = "jobprod.aspx";
                string aUsersviewjob = null;
                string PrmCompviewjob = null;
                bool vCanCreateviewjob = false;
                bool vCanRunviewjob = false;
                bool vCanUpdateviewjob = false;
                bool vCanDeleteviewjob = false;

                func1 f1viewjob = new func1();
                //Response.Write(Page);
                f1viewjob.CheckProgramPermissions(vPageviewjob, vUserIdviewjob, ref  vCanCreateviewjob, ref  vCanRunviewjob, ref  vCanUpdateviewjob, ref  vCanDeleteviewjob, ref  PrmCompviewjob, ref  aUsersviewjob);

                lblComp.Text = PrmCompviewjob;
                //Response.Write(vCanRun);
                if (vCanRunviewjob == true)
                {
                    lnk_viewjob.Visible = true;
                    liviewjob.Visible = true;

                }

                if (vCanRunviewjob == false)
                {
                    lnk_viewjob.Visible = false;
                    liviewjob.Visible = false;


                }

                string vUserIdmaterial = UserLogin.UserName;
                string vPagematerial = "material.aspx";
                string aUsersmaterial = null;
                string PrmCompmaterial = null;
                bool vCanCreatematerial = false;
                bool vCanRunmaterial = false;
                bool vCanUpdatematerial = false;
                bool vCanDeletematerial = false;

                func1 f1material = new func1();
                //Response.Write(Page);
                f1material.CheckProgramPermissions(vPagematerial, vUserIdmaterial, ref  vCanCreatematerial, ref  vCanRunmaterial, ref  vCanUpdatematerial, ref  vCanDeletematerial, ref  PrmCompmaterial, ref  aUsersmaterial);

                lblComp.Text = PrmCompmaterial;
                //Response.Write(vCanRun);
                if (vCanRunmaterial == true)
                {
                    lnk_material.Visible = true;
                    //material.Visible = true;

                }

                if (vCanRunmaterial == false)
                {
                    lnk_material.Visible = false;
                    //material.Visible = false;


                }

                string vUserIdmaterialinfo = UserLogin.UserName;
                string vPagematerialinfo = "materialinfo.aspx";
                string aUsersmaterialinfo = null;
                string PrmCompmaterialinfo = null;
                bool vCanCreatematerialinfo = false;
                bool vCanRunmaterialinfo = false;
                bool vCanUpdatematerialinfo = false;
                bool vCanDeletematerialinfo = false;

                func1 f1materialinfo = new func1();
                //Response.Write(Page);
                f1materialinfo.CheckProgramPermissions(vPagematerialinfo, vUserIdmaterialinfo, ref  vCanCreatematerialinfo, ref  vCanRunmaterialinfo, ref  vCanUpdatematerialinfo, ref  vCanDeletematerialinfo, ref  PrmCompmaterialinfo, ref  aUsersmaterialinfo);

                lblComp.Text = PrmCompmaterialinfo;
                //Response.Write(vCanRun);
                if (vCanRunmaterialinfo == true)
                {
                    lnk_matlinfo.Visible = true;
                    //matlinfo.Visible = true;

                }

                if (vCanRunmaterialinfo == false)
                {
                    lnk_matlinfo.Visible = false;
                   // matlinfo.Visible = false;


                }

                string vUserIdmachhrs = UserLogin.UserName;
                string vPagemachhrs = "machhrs.aspx";
                string aUsersmachhrs = null;
                string PrmCompmachhrs = null;
                bool vCanCreatemachhrs = false;
                bool vCanRunmachhrs = false;
                bool vCanUpdatemachhrs = false;
                bool vCanDeletemachhrs = false;

                func1 f1machhrs = new func1();
                //Response.Write(Page);
                f1machhrs.CheckProgramPermissions(vPagemachhrs, vUserIdmachhrs, ref  vCanCreatemachhrs, ref  vCanRunmachhrs, ref  vCanUpdatemachhrs, ref  vCanDeletemachhrs, ref  PrmCompmachhrs, ref  aUsersmachhrs);

                lblComp.Text = PrmCompmachhrs;
                //Response.Write(vCanRun);
                if (vCanRunmachhrs == true)
                {
                    lnk_mach_hrs.Visible = true;
                    //machhrs.Visible = true;

                }

                if (vCanRunmachhrs == false)
                {
                    lnk_mach_hrs.Visible = false;
                    //machhrs.Visible = false;


                }

                string vUserIdMachQty = UserLogin.UserName;
                string vPageMachQty = "MachQty.aspx";
                string aUsersMachQty = null;
                string PrmCompMachQty = null;
                bool vCanCreateMachQty = false;
                bool vCanRunMachQty = false;
                bool vCanUpdateMachQty = false;
                bool vCanDeleteMachQty = false;

                func1 f1MachQty = new func1();
                //Response.Write(Page);
                f1MachQty.CheckProgramPermissions(vPageMachQty, vUserIdMachQty, ref  vCanCreateMachQty, ref  vCanRunMachQty, ref  vCanUpdateMachQty, ref  vCanDeleteMachQty, ref  PrmCompMachQty, ref  aUsersMachQty);

                lblComp.Text = PrmCompMachQty;
                //Response.Write(vCanRun);
                if (vCanRunMachQty == true)
                {
                    lnk_machqtys.Visible = true;
                    //machqtys.Visible = true;

                }

                if (vCanRunMachQty == false)
                {
                    lnk_machqtys.Visible = false;
                    //machqtys.Visible = false;


                }
                string vUserIdMachWaste = UserLogin.UserName;
                string vPageMachWaste = "MachWaste.aspx";
                string aUsersMachWaste = null;
                string PrmCompMachWaste = null;
                bool vCanCreateMachWaste = false;
                bool vCanRunMachWaste = false;
                bool vCanUpdateMachWaste = false;
                bool vCanDeleteMachWaste = false;

                func1 f1MachWaste = new func1();
                //Response.Write(Page);
                f1MachWaste.CheckProgramPermissions(vPageMachWaste, vUserIdMachWaste, ref  vCanCreateMachWaste, ref  vCanRunMachWaste, ref  vCanUpdateMachWaste, ref  vCanDeleteMachWaste, ref  PrmCompMachWaste, ref  aUsersMachWaste);

                lblComp.Text = PrmCompMachWaste;
                //Response.Write(vCanRun);
                if (vCanRunMachWaste == true)
                {
                    lnk_waste.Visible = true;
                    //waste.Visible = true;

                }

                if (vCanRunMachWaste == false)
                {
                    lnk_waste.Visible = false;
                    //waste.Visible = false;


                }

            }
        }
    }
    protected void lnk_matlinfo_Click(object sender, EventArgs e)
    {
        //Response.Write(Session["order"]);
        //Response.Write(Session["item"]);
        if (Session["order"] != null)
        {
            Session["matinfo"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("materialinfo.aspx");
        }

    }
    protected void lnk_material_Click(object sender, EventArgs e)
    {
        //Response.Write(Session["order"]);
        //Response.Write(Session["item"]);
        if (Session["order"] != null)
        {
            Session["Material"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("material.aspx");
        }

    }
    protected void lnk_viewjob_Click(object sender, EventArgs e)
    {
        //Response.Write(Session["order"]);
        //Response.Write(Session["item"]);
        if (Session["order"] != null)
        {
            Session["Material"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("JobProd.aspx");
        }

    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL =

ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" +

"Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    protected void lnk_mach_hrs_Click(object sender, EventArgs e)
    {
        //Response.Write(Session["order"]);
        if (Session["order"] != null)
        {
            Session["machhr"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("machhrs.aspx");
        }
    }

    protected void lnk_fixedoh_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["MachFixOH"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("MachFixOH.aspx");
        }
    }
    protected void lnk_machcosts_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["MachCosts"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("MachCosts.aspx");
        }
    }
    protected void lnk_dl_Click(object sender, EventArgs e)
    {
        if (Session["order"] != null)
        {
            Session["MachDL"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("MachDL.aspx");
        }
    }
    protected void lnk_varoh_Click(object sender, EventArgs e)
    {
        {
            Session["MachVarOH"] = Session["order"];
            Session["line"] = Session["line"];
            Response.Redirect("MachVarOH.aspx");
        }
    }
    protected void lnk_waste_Click(object sender, EventArgs e)
    {
        Session["MachWaste"] = Session["order"];
        Session["line"] = Session["line"];
        Response.Redirect("MachWaste.aspx");
    }
    protected void lnk_machqtys_Click(object sender, EventArgs e)
    {
        Session["MachQty"] = Session["order"];
        Session["line"] = Session["line"];
        Response.Redirect("MachQty.aspx");
    }
    protected void viewjob_Click(object sender, ImageClickEventArgs e)
    {
        lnk_viewjob_Click(sender, e);
    }
    protected void material_Click(object sender, ImageClickEventArgs e)
    {
        lnk_material_Click(sender, e);
    }
    protected void matlinfo_Click(object sender, ImageClickEventArgs e)
    {
        lnk_matlinfo_Click(sender, e);
    }
    protected void machhrs_Click(object sender, ImageClickEventArgs e)
    {
        lnk_mach_hrs_Click(sender, e);
    }
    protected void machqtys_Click(object sender, ImageClickEventArgs e)
    {
        lnk_machqtys_Click(sender, e);
    }
    protected void waste_Click(object sender, ImageClickEventArgs e)
    {
        lnk_waste_Click(sender, e);
    }

    protected void lnk_brwsjobs_Click(object sender, EventArgs e)
    {
        Response.Redirect("brwsjobs.aspx");
    }
    protected void brwsjobs_Click(object sender, ImageClickEventArgs e)
    {
        lnk_brwsjobs_Click(sender, e);
    }
}
