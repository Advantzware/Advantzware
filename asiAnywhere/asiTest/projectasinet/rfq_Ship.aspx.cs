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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqship : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        Session["my_new_rfq"] = null;
        
        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function getvalue(e){");
        str.Append("document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTrnoTextBox.value=e[0];");
        str.Append("document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vPalletTextBox.value=e[1];}");
        str.Append("</script>");
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "getvalue"))
        {
            Page.RegisterClientScriptBlock("getvalue", str.ToString());
        }
        FormView1.ChangeMode(FormViewMode.ReadOnly);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "rfq_ship.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                //lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    //lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
            //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";

            //ImageButton ship = (ImageButton)Master.FindControl("rfq_shipping");
            //ship.ImageUrl = "~/Images/rfqshipping1.jpg";


        }
    }
    protected void UpdateButton_click(object sender, EventArgs e)
    {
        TextBox Ship = (TextBox)FormView1.FindControl("vShipidTextBox");
        TextBox name = (TextBox)FormView1.FindControl("VShipnameTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vCarrierTextBox");
        TextBox desc = (TextBox)FormView1.FindControl("vCarrdscrTextBox");
        TextBox casno = (TextBox)FormView1.FindControl("vCasnoTextBox");
        TextBox Trno = (TextBox)FormView1.FindControl("VTrnoTextBox");
        TextBox weight = (TextBox)FormView1.FindControl("vWeightTextBox");
        TextBox cascost = (TextBox)FormView1.FindControl("vCasCostTextBox");
        TextBox trcost = (TextBox)FormView1.FindControl("vTrCostTextBox");
        TextBox cascnt = (TextBox)FormView1.FindControl("vCascntTextBox");
        TextBox trcnt = (TextBox)FormView1.FindControl("vTrcntTextBox");
        TextBox clen = (TextBox)FormView1.FindControl("vCaslenTextBox");
        TextBox tlen = (TextBox)FormView1.FindControl("vTrlenTextBox");
        TextBox twid = (TextBox)FormView1.FindControl("vTrwidTextBox");
        TextBox cwid = (TextBox)FormView1.FindControl("vCasWidTextBox");
        TextBox cdep = (TextBox)FormView1.FindControl("vCasdepTextBox");
        TextBox tdep = (TextBox)FormView1.FindControl("vTrdepTextBox");
        TextBox cpal = (TextBox)FormView1.FindControl("vCaspalTextBox");
        TextBox trcas = (TextBox)FormView1.FindControl("vTrcasTextBox");
        TextBox caswt = (TextBox)FormView1.FindControl("vCaswtTextBox");

        //FormView1.ChangeMode(FormViewMode.Edit);

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRfqShipping";

        ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = Ship.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = desc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCasno"].DefaultValue = casno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTrno"].DefaultValue = Trno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWeight"].DefaultValue = weight.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmTrCost"].DefaultValue = trcost.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmCasCost"].DefaultValue =  cascost.Text.Trim();

        ObjectDataSource1.SelectParameters["prmCascnt"].DefaultValue = cascnt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTrcnt"].DefaultValue = trcnt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCaslen"].DefaultValue = clen.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTrlen"].DefaultValue = tlen.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCasWid"].DefaultValue = cwid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTrWid"].DefaultValue = twid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCasdep"].DefaultValue = cdep.Text.Trim();
        ObjectDataSource1.SelectParameters["prTrdep"].DefaultValue = tdep.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCaspal"].DefaultValue = cpal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTrcas"].DefaultValue = trcas.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCaswt"].DefaultValue = caswt.Text.Trim();
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "rfq_ship.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    if (aUsers == "external")
                    {
                        TextBox caspallet = (TextBox)FormView1.FindControl("vCaspalTextBox");
                        TextBox layer = (TextBox)FormView1.FindControl("vTrcasTextBox");
                        TextBox lbs = (TextBox)FormView1.FindControl("vCaswtTextBox");
                        TextBox wpm = (TextBox)FormView1.FindControl("vWeightTextBox");
                        caspallet.Enabled = false;
                        layer.Enabled = false;
                        lbs.Enabled = false;
                        wpm.Enabled = false;
                    }
                }
                TextBox ship = (TextBox)FormView1.FindControl("vShipidTextBox");

                if (ship.Text.Trim() != "")
                {
                    Session["carrier_lookup_val"] = ship.Text;
                }
                else
                {
                    Session["carrier_lookup_val"] = null;
                }
                ship.Focus();
            }
        }

        catch { return; }
    }
}